SUBDIRS = lisp dcc

install-package package:
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@) \
	done
