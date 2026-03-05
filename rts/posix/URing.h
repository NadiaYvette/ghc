/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2024-2025
 *
 * An I/O manager based on the Linux io_uring API.
 *
 * Prototypes for functions in URing.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#include "BeginPrivate.h"

#if defined(IOMGR_ENABLED_URING)

void initCapabilityIOManagerURing(CapIOManager *iomgr);

/* Synchronous I/O and timer operations */
bool syncIOWaitReadyURing(Capability *cap, StgTSO *tso,
                          IOReadOrWrite rw, HsInt fd);
void syncIOCancelURing(Capability *cap, StgTSO *tso);

/* Asynchronous operations */
bool asyncIOWaitReadyURing(Capability *cap, StgAsyncIOOp *aiop,
                           IOReadOrWrite rw, int fd);
void asyncIOCancelURing(Capability *cap, StgAsyncIOOp *aiop);

/* Scheduler operations */
bool anyPendingTimeoutsOrIOURing(CapIOManager *iomgr);
void pollCompletedTimeoutsOrIOURing(Capability *cap);
void awaitCompletedTimeoutsOrIOURing(Capability *cap);

/* Cleanup */
void closeCapabilityIOManagerURing(CapIOManager *iomgr);

#endif /* IOMGR_ENABLED_URING */

#include "EndPrivate.h"
