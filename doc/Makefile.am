EXTRA_DIST = texinfo.tex infohack.el ptexinfmt.el irchat-copyright.el

info_TEXINFOS = riece-ja.texi riece-en.texi
riece_ja_TEXINFOS = fdl.texi

package: all
	$(MAKE) EMACS=$(XEMACS)

install-package: package
	$(MAKE) infodir=$(PACKAGEDIR)/info install

%.info: %.texi
	$(EMACS) -batch -q -no-site-file -l ./infohack.el -f batch-makeinfo $<
