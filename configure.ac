AC_INIT(configure.ac)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(riece, 0.0.1)

AC_PROG_CC
AC_ISC_POSIX
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CHECK_HEADERS(sys/select.h libgen.h)

AC_CHECK_LIB(nsl, gethostbyname)
AC_CHECK_LIB(socket, socket)

AC_CHECK_FUNCS(strtoul memmove getaddrinfo)
AC_CHECK_FUNC(getopt_long,,[AC_LIBOBJ(getopt) AC_LIBOBJ(getopt1)])
AC_REPLACE_FUNCS(basename)
AC_CHECK_EMACS
AC_PATH_LISPDIR
AC_PATH_PACKAGEDIR

AC_OUTPUT(Makefile \
lisp/Makefile \
dcc/Makefile)
