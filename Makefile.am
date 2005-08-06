SUBDIRS = lisp doc
EXTRA_DIST = INSTALL-CVS README.ja NEWS.ja HACKING.ja
AUTOMAKE_OPTIONS = no-dependencies

if XEMACS
install-package package:
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@) \
	done
endif
