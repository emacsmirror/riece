EXTRA_DIST = COMPILE ChangeLog ChangeLog.Liece \
	riece-000.el riece-200.el riece-300.el riece-400.el riece-500.el \
	riece-channel.el riece-coding.el riece-commands.el riece-compat.el \
	riece-complete.el riece-display.el riece-emacs.el riece-filter.el \
	riece-globals.el riece-handle.el riece-highlight.el riece-identity.el \
	riece-message.el riece-misc.el riece-naming.el \
	riece-options.el riece-server.el riece-user.el riece-version.el \
	riece-xemacs.el riece.el \
	riece-ctcp.el riece-url.el riece-unread.el \
	riece-ndcc.el riece-rdcc.el riece-log.el riece-mini.el \
	riece-doctor.el riece-alias.el riece-layout.el riece-skk-kakutei.el \
	riece-guess.el riece-history.el riece-button.el riece-keyword.el

CLEANFILES = auto-autoloads.el custom-load.el *.elc
FLAGS ?= -batch -q -no-site-file

all: elc

elc:
	$(EMACS) $(FLAGS) -l COMPILE -f riece-compile

install: elc
	$(EMACS) $(FLAGS) -l COMPILE -f riece-install $(lispdir) # $(MAKE)

package:
	$(XEMACS) $(FLAGS) -l COMPILE -f riece-compile-package

install-package: package
	$(XEMACS) $(FLAGS) -l COMPILE -f riece-install-package \
		$(PACKAGEDIR) # $(MAKE)
