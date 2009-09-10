AC_INIT([riece], [6.1.0], [liece@unixuser.org])
AC_CONFIG_SRCDIR([configure.ac])
AC_PREREQ(2.63)
AM_INIT_AUTOMAKE

AC_CHECK_EMACS
AC_PATH_LISPDIR
AC_PATH_PACKAGEDIR

AM_CONDITIONAL(XEMACS, test ${EMACS_FLAVOR} = xemacs)

AC_CONFIG_FILES([Makefile
lisp/Makefile
lisp/riece-package-info.el
lisp/test/Makefile
doc/Makefile])
AC_OUTPUT
