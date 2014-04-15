SUBDIRS = lisp doc
AUTOMAKE_OPTIONS = no-dependencies

if XEMACS
install-package package:
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@) \
	done
endif

changelog_etc =					\
	ChangeLog-2014				\
	lisp/ChangeLog-2014			\
	lisp/ChangeLog.Liece

EXTRA_DIST =					\
  README.ja					\
  NEWS.ja					\
  $(changelog_etc)				\
  bootstrap					\
  bootstrap.conf

# Arrange so that .tarball-version appears only in the distribution
# tarball, and never in a checked-out repository.
# The perl substitution is to change some key uses of "rm" to "/bin/rm".
# See the rm_subst comment for details.
dist-hook: gen-ChangeLog
	$(AM_V_GEN)echo $(VERSION) > $(distdir)/.tarball-version
	$(AM_V_at)perl -pi -e '$(rm_subst)' $(distdir)/Makefile.in

gen_start_date = 2014-04-12
.PHONY: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
	    --amend=$(srcdir)/build-aux/git-log-fix			\
	    --since=$(gen_start_date) > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;			\
	fi
