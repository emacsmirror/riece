EXTRA_DIST = texinfo.tex fdl.texi riece-ja.texi

all: riece-ja.info

package: all
	$(MAKE) EMACS=$(XEMACS)

install-package: package
	$(MAKE) infodir=$(PACKAGEDIR)/info install

%-ja.info: %-ja.texi
	$(EMACS) -batch -q -no-site-file -l texinfmt \
		-f batch-texinfo-format $<
