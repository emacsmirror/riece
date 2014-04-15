AC_PREREQ(2.63)

AC_INIT([riece],
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
	[riece-discuss@nongnu.org])

AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_AUX_DIR([build-aux])

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
