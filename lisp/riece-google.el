;;; riece-google.el --- search keywords by Google
;; Copyright (C) 2005 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;;         SASADA Koichi <ko1 at atdot.net>
;; Keywords: IRC, riece

;; This file is part of Riece.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;; Ruby code was stolen (and modified) from nadoka.

;;; Code:

(require 'riece-message)

(defgroup riece-google nil
  "Search keywords by Google."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-google-ruby-command "ruby"
  "Command name for Ruby interpreter."
  :type 'string
  :group 'riece-google)

(defcustom riece-google-program
  '("\
# Copyright (c) 2004 SASADA Koichi <ko1 at atdot.net>
#
# This program is free software with ABSOLUTELY NO WARRANTY.
# You can re-distribute and/or modify this program under
# the same terms of the Ruby's lisence.

require 'soap/wsdlDriver'
require 'iconv'
require 'kconv'
require 'cgi'

keywords            = '" keywords "'
max_results         = " max-results "
license_key         = '" license-key "'
default_lang        = '" lang "'
google_wsdl         = 'http://api.google.com/GoogleSearch.wsdl'
google              = SOAP::WSDLDriverFactory.new(google_wsdl).create_driver

def erace_tag str
  CGI.unescapeHTML(str.gsub(/\\<.+?\\>/, ''))
end

def lang_check lang
  if lang.empty? || /^lang_/ =~ lang
    lang
  else
    'lang_' + lang
  end
end

def show_char_code_and_erace_tag str
  case $KCODE
  when 'EUC', 'SJIS'
    CGI.unescapeHTML(str.gsub(/\\<.+?\\>/, '')).tojis
  when 'NONE', 'UTF-8'
    begin
      str = Iconv.conv(\"EUC-JP\", \"UTF-8\", str)
      CGI.unescapeHTML(str.gsub(/\\<.+?\\>/, '')).tojis
    rescue => e
      \"(char code problem: #{e.class}[#{e.message.dump}])\"
    end
  else
    str
  end
end

def search_char_code str
  case $KCODE
  when 'EUC', 'SJIS'
    str.toeuc
  when 'NONE'
    begin
      Iconv.conv(\"UTF-8\", \"EUC-JP\", str.toeuc)
    rescue => e
      \"(char code problem: #{e.class})\"
    end
  when 'UTF-8'
    str
  else
    raise
  end
end

begin
  lang = lang_check(default_lang)
  word = search_char_code(keywords)
  result = google.doGoogleSearch(
    license_key, word, 0, max_results, false, \"\",
    false, lang, 'utf-8', 'utf-8'
  )

  count = result.estimatedTotalResultsCount
  if count > 0
    word = show_char_code_and_erace_tag(keywords)
    count = count.to_s.gsub(/(\\d)(?=\\d{3}+$)/, '\\\\1,')
    time = result.searchTime.to_s
    print \"Search results for #{word} (Hits: #{count}: Time: #{time}):\\n\"

    result.resultElements.each_with_index{|e, i|
      title = show_char_code_and_erace_tag(e.title)
      url   = e.URL
      print \"#{title} - #{url}\\n\"
    }
  else
    print \"no match\\n\"
  end

rescue Exception => e
  print \"#{e.class}(#{e.message})\"
end
")
  "Ruby program for searching by Google."
  :type 'sexp
  :group 'riece-google)

(defcustom riece-google-license-key nil
  "*License key for Google API."
  :type 'string
  :group 'riece-google)

(defcustom riece-google-default-lang '("lang_en" "lang_ja")
  "*Default language for searching keywords."
  :type '(repeat (choice (const "lang_en" :tag "English")
			 (const "lang_ja" :tag "Japanese")
			 (string :tag "The other language")))
  :group 'riece-google)

(defconst riece-google-regexp
  "^go\\(o+\\)gle\\(:\\([a-z]+\\)\\)?>\\s-*\\(.*\\)")

(defconst riece-google-description
  "Search keywords by Google.")

(defvar riece-google-target nil)

(defun riece-google-display-message-function (message)
  (when (and (get 'riece-google 'riece-addon-enabled)
	     (stringp riece-google-license-key)
	     (string-match riece-google-regexp (riece-message-text message)))
    (let ((keywords (match-string 4 (riece-message-text message)))
	  (max-results (number-to-string
			(length
			 (match-string 1 (riece-message-text message)))))
	  (lang (or (match-string 3 (riece-message-text message))
		    riece-google-default-lang))
	  (process-connection-type nil)
	  selective-display
	  (coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (process (start-process "Google" (generate-new-buffer " *Google*")
				  riece-google-ruby-command)))
      (when (listp lang)
	(setq lang (mapconcat #'identity lang " ")))
      (setq riece-google-target (riece-message-target message))
      (process-send-string process
			   (apply #'concat
				  (riece-google-substitute-variables
				   (riece-google-substitute-variables
				    (riece-google-substitute-variables
				     (riece-google-substitute-variables
				      riece-google-program
				      'keywords keywords)
				     'max-results max-results)
				     'license-key riece-google-license-key)
				   'lang lang)))
      (process-send-eof process)
      (with-current-buffer (process-buffer process)
	(set-buffer-multibyte t)
	(erase-buffer)
      (set-buffer-modified-p nil))
      (set-process-filter process #'riece-google-filter)
      (set-process-sentinel process #'riece-google-sentinel))))

(defun riece-google-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)
      (goto-char (point-min))
      (while (progn (end-of-line) (and (not (eobp)) (eq (char-after) ?\n)))
	(if (eq (char-after (1- (point))) ?\r) ; cut off preceding LF
	    (delete-region (1- (point)) (point)))
	(riece-google-send-string riece-google-target
				  (buffer-substring (point-min) (point)))
	(delete-region (point-min) (progn (beginning-of-line 2) (point)))))))

(defun riece-google-sentinel (process string)
  (delete-process process))

(defun riece-google-send-string (target message)
  (riece-send-string
   (format "NOTICE %s :%s\r\n" (riece-identity-prefix target) message))
  (riece-display-message
   (riece-make-message (riece-current-nickname) target message 'notice)))

(defun riece-google-substitute-variables (program variable value)
  (setq program (copy-sequence program))
  (let ((pointer program))
    (while pointer
      (setq pointer (memq variable program))
      (if pointer
	  (setcar pointer value)))
    program))

(defun riece-google-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-google-display-message-function))

(defun riece-google-uninstall ()
  (remove-hook 'riece-after-display-message-functions
	       'riece-google-display-message-function))

(provide 'riece-google)

;;; riece-google.el ends here
