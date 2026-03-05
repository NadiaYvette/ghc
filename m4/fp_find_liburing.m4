# FP_FIND_LIBURING
# --------------------------------------------------------------
# Should we use liburing? (yes, no, or auto.)
#
# Sets variables:
#   - UseLibURing: [YES|NO]
#   - LibUringLibDir: optional path
#   - LibUringIncludeDir: optional path
AC_DEFUN([FP_FIND_LIBURING],
[
  AC_ARG_WITH([liburing-libraries],
    [AS_HELP_STRING([--with-liburing-libraries=ARG],
      [Find libraries for liburing in ARG [default=system default]])
    ],
    [
      LibUringLibDir="$withval"
      LIBURING_LDFLAGS="-L$withval"
    ])

  AC_ARG_WITH([liburing-includes],
    [AS_HELP_STRING([--with-liburing-includes=ARG],
      [Find includes for liburing in ARG [default=system default]])
    ],
    [
      LibUringIncludeDir="$withval"
      LIBURING_CFLAGS="-I$withval"
    ])

  AC_ARG_ENABLE(uring,
    [AS_HELP_STRING([--enable-uring],
      [Enable io_uring-based I/O manager in the runtime system
       via liburing [default=auto]])],
    [],
    [enable_uring=auto])

  UseLibURing=NO
  if test "$enable_uring" != "no" ; then
    CFLAGS2="$CFLAGS"
    CFLAGS="$LIBURING_CFLAGS $CFLAGS"
    LDFLAGS2="$LDFLAGS"
    LDFLAGS="$LIBURING_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([liburing.h])

    if test "$ac_cv_header_liburing_h" = "yes" ; then
      AC_CHECK_LIB([uring], [io_uring_queue_init], [UseLibURing=YES])
    fi
    if test "$enable_uring:$UseLibURing" = "yes:NO" ; then
      AC_MSG_ERROR([Cannot find system liburing (required by --enable-uring)])
    fi

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi
])
