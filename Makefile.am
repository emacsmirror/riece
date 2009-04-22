SUBDIRS = lisp doc
EXTRA_DIST = README.ja NEWS.ja
AUTOMAKE_OPTIONS = no-dependencies

if XEMACS
install-package package:
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@) \
	done
endif
