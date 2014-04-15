AUTOMAKE_OPTIONS = -Wno-portability
EXTRA_DIST = infohack.el ptexinfmt.el irchat-copyright.el

info_TEXINFOS = riece-ja.texi riece-en.texi

package: all
	$(MAKE) EMACS=$(XEMACS)

install-package: package
	$(MAKE) infodir=$(PACKAGEDIR)/info install

SUFFIXES = .texi .info
.texi.info:
	$(EMACS) -batch -q -no-site-file -l $(srcdir)/infohack.el $(srcdir) \
		-f batch-makeinfo $<
