AC_INIT(configure.ac)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(riece, 1.0.5)

AC_ISC_POSIX
AC_PROG_INSTALL
AC_PROG_MAKE_SET

AC_CHECK_EMACS
AC_PATH_LISPDIR
AC_PATH_PACKAGEDIR

AM_CONDITIONAL(XEMACS, test ${EMACS_FLAVOR} = xemacs)

AC_EMACS_LISP(mule, (if (featurep (quote mule)) \"yes\" \"no\"), "noecho")
AM_CONDITIONAL(XEMACS_NOMULE, test $mule = "no")

AC_OUTPUT(Makefile \
lisp/Makefile
lisp/test/Makefile
doc/Makefile)
