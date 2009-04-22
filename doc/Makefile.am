AUTOMAKE_OPTIONS = -Wno-portability
EXTRA_DIST = texinfo.tex infohack.el ptexinfmt.el irchat-copyright.el \
	HACKING HACKING.ja

info_TEXINFOS = riece-ja.texi riece-en.texi

package: all
	$(MAKE) EMACS=$(XEMACS)

install-package: package
	$(MAKE) infodir=$(PACKAGEDIR)/info install

%.info: %.texi
	$(EMACS) -batch -q -no-site-file -l ./infohack.el -f batch-makeinfo $<
