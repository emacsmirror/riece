(require 'riece-mcat)

(defconst riece-addon-modules
  (list
   (cons 'riece-alias
	 (riece-mcat "Define aliases for IRC names."))
   (cons 'riece-async
	 (riece-mcat "Connect to IRC server via async proxy."))
   (cons 'riece-biff
	 (riece-mcat "Be notified if messages arrives."))
   (cons 'riece-button
	 (riece-mcat "Display useful buttons in IRC buffers."))
   (cons 'riece-ctcp
	 (riece-mcat "CTCP (Client To Client Protocol) support."))
   (cons 'riece-ctlseq
	 (riece-mcat "Mark up control sequences in IRC buffers."))
   (cons 'riece-desktop-notify
	 (riece-mcat "Display notification to desktop."))
   (cons 'riece-doctor
	 (riece-mcat "Pretend to be a psychotherapist."))
   (cons 'riece-epg
	 (riece-mcat "Encrypt/decrypt messages."))
   (cons 'riece-eval-ruby
	 (riece-mcat "Evaluate input string as a Ruby program."))
   (cons 'riece-eval
	 (riece-mcat "Evaluate an input string as an elisp form."))
   (cons 'riece-foolproof
	 (riece-mcat "Prevent miss-operation in the command buffer."))
   (cons 'riece-google
	 (riece-mcat "Search keywords by Google."))
   (cons 'riece-guess
	 (riece-mcat "Guess the next channel, using multiple methods."))
   (cons 'riece-hangman
	 (riece-mcat "Allow channel members to play the hangman game."))
   (cons 'riece-highlight
	 (riece-mcat "Highlight IRC buffers."))
   (cons 'riece-history
	 (riece-mcat "Manage history of channel shifting."))
   (cons 'riece-icon
	 (riece-mcat "Display icons in IRC buffers."))
   (cons 'riece-ignore
	 (riece-mcat "Ignore messages from some users."))
   (cons 'riece-kakasi
	 (riece-mcat "Convert Japanese to roman string by KAKASI."))
   (cons 'riece-keepalive
	 (riece-mcat "Keep an IRC connection."))
   (cons 'riece-keyword
	 (riece-mcat "Detect keywords in IRC buffers."))
   (cons 'riece-log
	 (riece-mcat "Save IRC logs."))
   (cons 'riece-lsdb
	 (riece-mcat "Help register nicknames in LSDB rolodex program."))
   (cons 'riece-mcat
	 (riece-mcat "Translate messages."))
   (cons 'riece-menu
	 (riece-mcat "Setup Riece's command menus."))
   (cons 'riece-mini
	 (riece-mcat "Use Riece only on the minibuffer."))
;;;    (cons 'riece-ndcc
;;; 	 (riece-mcat "DCC file sending protocol support (written in elisp.)"))
   (cons 'riece-rdcc
	 (riece-mcat "DCC file sending protocol support (written in Ruby.)"))
   (cons 'riece-shrink-buffer
	 (riece-mcat "Free old IRC messages to save memory usage."))
   (cons 'riece-skk-kakutei
	 (riece-mcat "Remove SKK's preedit mark before sending messages."))
   (cons 'riece-toolbar
	 (riece-mcat "Display toolbar icons."))
   (cons 'riece-twitter
	 (riece-mcat "Send your status to Twitter."))
   (cons 'riece-unread
	 (riece-mcat "Mark channels where new messages arrived."))
   (cons 'riece-url
	 (riece-mcat "Collect URL in IRC buffers."))
   (cons 'riece-xface
	 (riece-mcat "Display X-Face in IRC buffers."))
   (cons 'riece-xfaceb
	 (riece-mcat "Display X-Face & Colour Face images in IRC buffers \(BBDB\)."))
   (cons 'riece-yank
	 (riece-mcat "Enter the element of kill-ring."))))

(provide 'riece-addon-modules)
