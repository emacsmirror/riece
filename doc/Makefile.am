EXTRA_DIST = texinfo.tex

info_TEXINFOS = riece-ja.texi
riece_ja_TEXINFOS = fdl.texi

package: all
	$(MAKE) EMACS=$(XEMACS)

install-package: package
	$(MAKE) infodir=$(PACKAGEDIR)/info install

%-ja.info: %-ja.texi
	$(EMACS) -batch -q -no-site-file -l ./infohack.el -f batch-makeinfo $<
