SUBDIRS = lisp dcc
AUTOMAKE_OPTIONS = no-dependencies

install-package package:
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@) \
	done
