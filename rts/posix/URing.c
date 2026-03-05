/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2024-2025
 *
 * An I/O manager based on the Linux io_uring API.
 *
 * This I/O manager supports true asynchronous I/O completion, not just
 * readiness notification. It uses io_uring to submit read/write operations
 * directly to the kernel, and receives completion notifications when the
 * operations are done. This allows the kernel to perform I/O directly
 * to/from user-space buffers without the need for a separate read/write
 * system call after notification.
 *
 * For backward compatibility with the waitRead#/waitWrite# primops (which
 * are readiness-based), we use IORING_OP_POLL_ADD to poll for fd readiness.
 * This makes the uring backend a drop-in replacement for the poll backend
 * for existing code, while also supporting true async I/O for new code paths.
 *
 * The design follows the same pattern as Poll.c:
 * - Per-capability io_uring instance (via CapIOManager)
 * - ClosureTable of StgAsyncIOOps for tracking in-flight operations
 * - TimeoutQueue for threadDelay support
 * - The io_uring user_data field carries the ClosureTable index, allowing
 *   us to match completions back to their StgAsyncIOOp
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"

#include "IOManager.h"

#if defined(IOMGR_ENABLED_URING)

#include "Capability.h"
#include "Threads.h"
#include "Schedule.h"
#include "Prelude.h"
#include "RtsUtils.h"
#include "rts/Time.h"
#include "RaiseAsync.h"
#include "Trace.h"

#include "URing.h"
#include "RtsSignals.h"

#include <liburing.h>
#include <poll.h>
#include <errno.h>
#include <string.h>

#include "IOManagerInternals.h"
#include "Timeout.h"

/******************************************************************************

This I/O manager is based on the Linux io_uring API.

io_uring provides a shared memory interface between the kernel and user-space
for submitting I/O operations and receiving completions. It uses two ring
buffers: a submission queue (SQ) and a completion queue (CQ).

To submit I/O:
  1. Get a submission queue entry (SQE) from the ring
  2. Fill in the operation details (read, write, poll, etc.)
  3. Set the user_data field to identify the operation on completion
  4. Submit all pending SQEs to the kernel

To receive completions:
  1. Wait for or peek at completion queue entries (CQEs)
  2. Read the result and user_data from the CQE
  3. Mark the CQE as seen (consumed)

For readiness-based I/O (waitRead#/waitWrite#), we use IORING_OP_POLL_ADD
which asks the kernel to poll an fd for readiness, similar to poll(). This
gives us the same semantics as the poll I/O manager.

For true async I/O completion, we use IORING_OP_READ/IORING_OP_WRITE which
submit actual read/write operations. The StgAsyncIOOp.result field receives
the number of bytes transferred (or error code).

The user_data in each SQE is set to the ClosureTable index of the
StgAsyncIOOp. When a CQE arrives, we use this index to look up the aiop
and notify the waiting thread.

The CapIOManager structure for this I/O manager contains:

    struct io_uring  uring;
    ClosureTable     aiop_table;
    StgTimeoutQueue *timeout_queue;

******************************************************************************/

/* The default size of the submission queue ring. This will be rounded up to
 * the next power of two by the kernel. 256 is a reasonable default that
 * allows batching many operations.
 */
#define URING_SQ_SIZE 256

/* Forward declarations */
static bool enlargeTable(Capability *cap, CapIOManager *iomgr);
static void notifyIOCompletion(Capability *cap, StgAsyncIOOp *aiop);
static void ioCancel(Capability *cap, StgAsyncIOOp *aiop);
static void processCompletions(Capability *cap, CapIOManager *iomgr);


void initCapabilityIOManagerURing(CapIOManager *iomgr)
{
    initClosureTable(&iomgr->aiop_table, ClosureTableNonCompact);
    iomgr->timeout_queue = emptyTimeoutQueue();

    /* Initialise the io_uring.
     *
     * We use io_uring_queue_init which sets up both the SQ and CQ rings.
     * The CQ ring is by default twice the SQ ring size.
     *
     * We use no special flags for now. In future we could consider:
     * - IORING_SETUP_SQPOLL for kernel-side SQ polling (reduces syscalls)
     * - IORING_SETUP_IOPOLL for busy-polling completions
     */
    int ret = io_uring_queue_init(URING_SQ_SIZE, &iomgr->uring, 0);
    if (ret < 0) {
        sysErrorBelch("io_uring_queue_init failed: %s", strerror(-ret));
        stg_exit(EXIT_FAILURE);
    }
    iomgr->uring_initialized = true;
}


void closeCapabilityIOManagerURing(CapIOManager *iomgr)
{
    if (iomgr->uring_initialized) {
        io_uring_queue_exit(&iomgr->uring);
        iomgr->uring_initialized = false;
    }
}


/* Submit a POLL_ADD request to the io_uring for fd readiness notification.
 * This implements the semantics of waitRead#/waitWrite#.
 *
 * We use non-compact mode for the ClosureTable because io_uring identifies
 * operations by user_data (which we set to the table index), and we need
 * that index to be stable for the duration of the I/O operation.
 */
bool syncIOWaitReadyURing(Capability *cap, StgTSO *tso,
                          IOReadOrWrite rw, HsInt fd)
{
    StgAsyncIOOp *aiop;
    aiop = (StgAsyncIOOp *)allocateMightFail(cap, sizeofW(StgAsyncIOOp));
    if (RTS_UNLIKELY(aiop == NULL)) return false;
    SET_HDR(aiop, &stg_ASYNCIOOP_info, cap->r.rCCCS);
    aiop->notify.tso     = tso;
    aiop->notify_type    = NotifyTSO;
    aiop->live           = &stg_ASYNCIO_LIVE0_closure;
    tso->why_blocked     = rw == IORead ? BlockedOnRead : BlockedOnWrite;
    tso->block_info.aiop = aiop;
    return asyncIOWaitReadyURing(cap, aiop, rw, fd);
}


bool asyncIOWaitReadyURing(Capability *cap, StgAsyncIOOp *aiop,
                           IOReadOrWrite rw, int fd)
{
    CapIOManager *iomgr = cap->iomgr;
    if (RTS_UNLIKELY(isFullClosureTable(&iomgr->aiop_table))) {
        bool ok = enlargeTable(cap, iomgr);
        if (RTS_UNLIKELY(!ok)) return false;
    }

    int ix = insertClosureTable(cap, &iomgr->aiop_table, aiop);

    aiop->capno   = cap->no;
    aiop->index   = ix;
    aiop->outcome = IOOpOutcomeInFlight;

    /* Get a submission queue entry */
    struct io_uring_sqe *sqe = io_uring_get_sqe(&iomgr->uring);
    if (RTS_UNLIKELY(sqe == NULL)) {
        /* The SQ is full. We need to submit pending entries and try again. */
        io_uring_submit(&iomgr->uring);
        sqe = io_uring_get_sqe(&iomgr->uring);
        if (RTS_UNLIKELY(sqe == NULL)) {
            /* Still full after submit — this shouldn't happen with a
             * reasonably sized ring, but handle it gracefully. */
            removeClosureTable(cap, &iomgr->aiop_table, ix);
            return false;
        }
    }

    /* Use POLL_ADD to poll for fd readiness, matching poll() semantics. */
    unsigned poll_mask = rw == IORead ? POLLIN : POLLOUT;
    io_uring_prep_poll_add(sqe, fd, poll_mask);
    io_uring_sqe_set_data64(sqe, (uint64_t)ix);

    /* We don't submit immediately — we batch submissions and submit them
     * all at once when the scheduler calls pollCompletedTimeoutsOrIO or
     * awaitCompletedTimeoutsOrIO. However, since io_uring_enter is needed
     * to actually start the operations, we submit now to ensure the kernel
     * starts polling.
     */
    int ret = io_uring_submit(&iomgr->uring);
    if (RTS_UNLIKELY(ret < 0)) {
        if (ret == -EINTR) {
            /* Interrupted, but the SQE is queued. Try again. */
            ret = io_uring_submit(&iomgr->uring);
        }
        if (ret < 0) {
            removeClosureTable(cap, &iomgr->aiop_table, ix);
            return false;
        }
    }

    return true;
}


void syncIOCancelURing(Capability *cap, StgTSO *tso)
{
    StgAsyncIOOp *aiop = tso->block_info.aiop;
    ASSERT(aiop->notify_type == NotifyTSO);
    ASSERT(indexClosureTable(&cap->iomgr->aiop_table, aiop->index) == aiop);
    ioCancel(cap, aiop);
    tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
}


void asyncIOCancelURing(Capability *cap, StgAsyncIOOp *aiop)
{
    ASSERT(aiop->notify_type != NotifyTSO);
    if (indexClosureTable(&cap->iomgr->aiop_table, aiop->index) == aiop) {
        ioCancel(cap, aiop);
        notifyIOCompletion(cap, aiop);
    }
}


static void ioCancel(Capability *cap, StgAsyncIOOp *aiop)
{
    CapIOManager *iomgr = cap->iomgr;

    /* Mark the aiop as cancelled. We do NOT remove it from the ClosureTable
     * yet, because the kernel may still deliver a CQE for this operation
     * (either the original result or -ECANCELED). We'll clean it up when
     * we process that CQE in processCompletions.
     */
    aiop->outcome = IOOpOutcomeCancelled;

    /* Submit a cancellation request to io_uring.
     * IORING_OP_ASYNC_CANCEL will cancel an in-flight operation identified
     * by its user_data. This tells the kernel to try to cancel the op,
     * which will cause it to complete with -ECANCELED.
     */
    struct io_uring_sqe *sqe = io_uring_get_sqe(&iomgr->uring);
    if (sqe != NULL) {
        io_uring_prep_cancel64(sqe, (uint64_t)aiop->index, 0);
        /* Mark the cancel SQE with a sentinel user_data so we can ignore
         * its CQE when it completes. */
        io_uring_sqe_set_data64(sqe, UINT64_MAX);
        io_uring_submit(&iomgr->uring);
    }
    /* If we couldn't get an SQE for the cancel, that's ok — the original
     * operation will still complete eventually and we'll clean it up then.
     */
}


bool anyPendingTimeoutsOrIOURing(CapIOManager *iomgr)
{
    return !isEmptyTimeoutQueue(iomgr->timeout_queue)
        || !isEmptyClosureTable(&iomgr->aiop_table);
}


static void notifyIOCompletion(Capability *cap, StgAsyncIOOp *aiop)
{
    ASSERT(aiop->outcome != IOOpOutcomeInFlight);
    switch (aiop->notify_type) {
        case NotifyTSO:
        {
            if (aiop->outcome == IOOpOutcomeFailed && aiop->error == EBADF) {
                StgTSO *tso = aiop->notify.tso;
                debugTrace(DEBUG_iomanager,
                           "Raising exception in thread %" FMT_StgThreadID
                           " blocked on an invalid fd", tso->id);
                raiseAsync(cap, tso, (StgClosure *)blockedOnBadFD_closure,
                           false, NULL);
                break;
            } else {
                StgTSO *tso      = aiop->notify.tso;
                tso->why_blocked = NotBlocked;
                tso->_link       = END_TSO_QUEUE;
                pushOnRunQueue(cap, tso);
            }
            break;
        }
        case NotifyMVar:
            barf("uring iomgr: MVar notification not yet supported");
            break;

        case NotifyTVar:
            barf("uring iomgr: TVar notification not yet supported");
            break;
    }
}


/* Process all available completions from the io_uring CQ ring. */
static void processCompletions(Capability *cap, CapIOManager *iomgr)
{
    struct io_uring_cqe *cqe;
    unsigned head;
    unsigned completed = 0;

    io_uring_for_each_cqe(&iomgr->uring, head, cqe) {
        uint64_t user_data = io_uring_cqe_get_data64(cqe);

        /* Skip sentinel CQEs from cancellation requests and timeouts */
        if (user_data == UINT64_MAX) {
            completed++;
            continue;
        }

        int ix = (int)user_data;
        int32_t res = cqe->res;

        debugTrace(DEBUG_iomanager,
                   "io_uring completion: index=%d, result=%d", ix, res);

        /* Bounds check */
        if (ix < 0 || ix >= capacityClosureTable(&iomgr->aiop_table)) {
            completed++;
            continue;
        }

        /* Look up the aiop */
        StgAsyncIOOp *aiop = indexClosureTable(&iomgr->aiop_table, ix);

        /* Remove from the table now that the kernel is done with it */
        removeClosureTable(cap, &iomgr->aiop_table, ix);

        /* If this aiop was already marked as cancelled (by ioCancel),
         * we just silently drop the completion. The thread has already
         * been dealt with via the cancellation path.
         */
        if (aiop->outcome == IOOpOutcomeCancelled) {
            completed++;
            continue;
        }

        /* Fill in the outcome */
        if (res < 0) {
            aiop->outcome = IOOpOutcomeFailed;
            aiop->error   = (uint32_t)(-res);
        } else {
            aiop->outcome = IOOpOutcomeSuccess;
            aiop->result  = (uint32_t)res;
        }

        notifyIOCompletion(cap, aiop);
        completed++;
    }

    if (completed > 0) {
        io_uring_cq_advance(&iomgr->uring, completed);
    }
}


void pollCompletedTimeoutsOrIOURing(Capability *cap)
{
    CapIOManager *iomgr = cap->iomgr;

    if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
        Time now = getProcessElapsedTime();
        processTimeoutCompletions(cap, now);
    }

    if (!isEmptyClosureTable(&iomgr->aiop_table)) {
        /* Non-blocking: just peek at any available completions */
        processCompletions(cap, iomgr);
    }
}


void awaitCompletedTimeoutsOrIOURing(Capability *cap)
{
    CapIOManager *iomgr = cap->iomgr;

    do {
        ASSERT(!isEmptyTimeoutQueue(iomgr->timeout_queue) ||
               !isEmptyClosureTable(&iomgr->aiop_table));

        Time now = getProcessElapsedTime();
        processTimeoutCompletions(cap, now);

        bool wait = emptyRunQueue(cap);

        if (!isEmptyClosureTable(&iomgr->aiop_table)) {
            if (wait) {
                /* We need to block until I/O completes or a timeout fires.
                 * Use io_uring_wait_cqe_timeout if we have a timeout, or
                 * io_uring_wait_cqe for indefinite wait.
                 */
                struct io_uring_cqe *cqe;
                int ret;

                if (!isEmptyTimeoutQueue(iomgr->timeout_queue)) {
                    /* Compute timeout to next timer expiry */
                    struct __kernel_timespec ts;
                    Time next = findMinWaketimeTimeoutQueue(iomgr->timeout_queue);
                    Time delay = next - now;
                    if (delay < 0) delay = 0;

                    /* Time is already in nanoseconds (TIME_RESOLUTION) */
                    ts.tv_sec  = TimeToSeconds(delay);
                    ts.tv_nsec = TimeToNS(delay - SecondsToTime(ts.tv_sec));

                    ret = io_uring_wait_cqe_timeout(&iomgr->uring, &cqe, &ts);
                } else {
                    /* No timeouts pending, wait indefinitely for I/O */
                    ret = io_uring_wait_cqe(&iomgr->uring, &cqe);
                }

                debugTrace(DEBUG_iomanager,
                           "io_uring_wait_cqe returned %d", ret);

                if (ret == -EINTR) {
#if defined(RTS_USER_SIGNALS)
                    if (startPendingSignalHandlers(cap)) break;
#endif
                    /* Signal but not ours, loop round */
                } else if (ret == -ETIME) {
                    /* Timeout expired, loop round to process timeouts */
                } else if (ret < 0) {
                    sysErrorBelch("io_uring_wait_cqe: %s", strerror(-ret));
                    stg_exit(EXIT_FAILURE);
                }
                /* If ret == 0, there are completions available */
            }

            /* Process all available completions (blocking or non-blocking) */
            processCompletions(cap, iomgr);
        } else if (wait && !isEmptyTimeoutQueue(iomgr->timeout_queue)) {
            /* No I/O pending but we have timeouts. We need to sleep until the
             * next timeout. We can't use io_uring for this since there are no
             * SQEs. Use a simple nanosleep-style wait via io_uring timeout.
             */
            Time next = findMinWaketimeTimeoutQueue(iomgr->timeout_queue);
            Time delay = next - now;
            if (delay > 0) {
                struct __kernel_timespec ts;
                ts.tv_sec  = TimeToSeconds(delay);
                ts.tv_nsec = TimeToNS(delay - SecondsToTime(ts.tv_sec));

                /* Submit a timeout operation to io_uring and wait for it */
                struct io_uring_sqe *sqe = io_uring_get_sqe(&iomgr->uring);
                if (sqe != NULL) {
                    io_uring_prep_timeout(sqe, &ts, 0, 0);
                    io_uring_sqe_set_data64(sqe, UINT64_MAX);
                    io_uring_submit(&iomgr->uring);

                    struct io_uring_cqe *cqe;
                    int ret = io_uring_wait_cqe(&iomgr->uring, &cqe);
                    if (ret == 0) {
                        io_uring_cqe_seen(&iomgr->uring, cqe);
                    }
                    /* Ignore errors — we'll just loop and re-check timeouts */
                }
            }
        }

    } while (emptyRunQueue(cap)
         && (getSchedState() == SCHED_RUNNING));
}


static bool enlargeTable(Capability *cap, CapIOManager *iomgr)
{
    int oldcapacity = capacityClosureTable(&iomgr->aiop_table);
    int newcapacity = (oldcapacity == 0) ? 1 : (oldcapacity * 2);

    return enlargeClosureTable(cap, &iomgr->aiop_table, newcapacity);
}

#endif /* IOMGR_ENABLED_URING */
