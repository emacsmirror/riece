SUBDIRS = test

EXTRA_DIST = COMPILE ChangeLog ChangeLog.Liece \
	riece-000.el riece-200.el riece-300.el riece-400.el riece-500.el \
	riece-addon-modules.el riece-addon.el riece-cache.el riece-channel.el \
	riece-coding.el riece-commands.el riece-compat.el riece-complete.el \
	riece-debug.el riece-display.el riece-emacs.el riece-filter.el \
	riece-globals.el riece-handle.el riece-highlight.el riece-identity.el \
	riece-mcat.el riece-message.el riece-misc.el riece-mode.el \
	riece-naming.el riece-options.el riece-ruby.el riece-server.el \
	riece-signal.el riece-user.el riece-version.el riece-xemacs.el \
	riece-irc.el riece.el \
	riece-mcat-japanese.el \
	riece-ctcp.el riece-url.el riece-unread.el riece-ndcc.el \
	riece-rdcc.el riece-log.el riece-mini.el riece-doctor.el \
	riece-alias.el riece-layout.el riece-skk-kakutei.el riece-guess.el \
	riece-history.el riece-button.el riece-keyword.el riece-menu.el \
	riece-icon.el riece-async.el riece-lsdb.el riece-xface.el \
	riece-ctlseq.el riece-ignore.el riece-hangman.el riece-biff.el \
	riece-kakasi.el riece-foolproof.el riece-yank.el riece-toolbar.el \
	riece-eval.el riece-google.el riece-keepalive.el riece-eval-ruby.el \
	riece-shrink-buffer.el riece-xfaceb.el riece-epg.el riece-twitter.el \
	riece-desktop-notify.el \
	url-riece.el \
	riece-command-previous-channel.xpm riece-command-next-channel.xpm \
	riece-command-configure-windows.xpm riece-command-list-addons.xpm \
	riece-command-join.xpm riece-command-part.xpm \
	server.rb aproxy.rb

CLEANFILES = auto-autoloads.el custom-load.el *.elc lunit-report.xml
DISTCLEANFILES = riece-package-info.el
FLAGS ?= -batch -q -no-site-file

all: elc

elc:
	$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-compile \
		$(srcdir)

install: elc
	$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-install \
		$(srcdir) $(lispdir) # $(MAKE)

uninstall:
	$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-uninstall \
		$(lispdir)

package:
	$(XEMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-compile-package \
		$(srcdir)

install-package: package
	$(XEMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-install-package \
		$(srcdir) $(PACKAGEDIR) # $(MAKE)

check-local:
	$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-test \
		$(srcdir) lunit-report.xml

compile-individually:
	@for i in `$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-examine`; \
	do \
		echo $(EMACS) $(FLAGS) -l $(srcdir)/COMPILE \
			-f riece-compile-module $$i; \
		$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE \
			-f riece-compile-module $$i; \
	done

update-mcat:
	$(EMACS) $(FLAGS) -l $(srcdir)/COMPILE -f riece-update-mcat \
		$(srcdir)
