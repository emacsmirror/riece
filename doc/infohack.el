;;; infohack.el --- a hack to format info file.
;; Copyright (C)  2001  Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: info

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;(let ((default-directory (expand-file-name "../lisp/"))
;      (features (cons 'w3-forms (copy-sequence features))))
;  ;; Adjust `load-path' for APEL.
;  (load-file "dgnushack.el"))
(load-file (expand-file-name "ptexinfmt.el" "./"))

(defun infohack-remove-unsupported ()
  (goto-char (point-min))
  (while (re-search-forward "@\\(end \\)?ifnottex" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (search-forward "\n@iflatex\n" nil t)
    (delete-region (1+ (match-beginning 0))
		   (search-forward "\n@end iflatex\n"))))

(defun infohack (file)
  (let ((dest-directory default-directory)
	(max-lisp-eval-depth (max max-lisp-eval-depth 600))
	coding-system)
    (find-file file)
    (setq buffer-read-only nil)
    (setq coding-system (if (boundp 'buffer-file-coding-system)
			    buffer-file-coding-system
			  file-coding-system))
    (infohack-remove-unsupported)
    (texinfo-every-node-update)
    (texinfo-format-buffer t) ;; Don't save any file.
    (setq default-directory dest-directory)
    (setq buffer-file-name
	  (expand-file-name (file-name-nondirectory buffer-file-name)
			    default-directory))
    (setq buffer-file-coding-system coding-system
	  file-coding-system coding-system)
    (if (> (buffer-size) 100000)
	(Info-split))
    (save-buffer)))

(eval-and-compile
  (when (string-match "windows-nt\\|os/2\\|emx\\|cygwin"
                      (symbol-name system-type))
    (defun subst-char-in-region (START END FROMCHAR TOCHAR &optional NOUNDO)
      "From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form."
      (let ((original-buffer-undo-list buffer-undo-list)
            (modified (buffer-modified-p)))
        (if NOUNDO
            (setq buffer-undo-list t))
        (goto-char START)
        (let ((from (char-to-string FROMCHAR))
              (to (char-to-string TOCHAR)))
          (while (search-forward from END t)
            (replace-match to t t)))
        (if NOUNDO
            (progn (setq buffer-undo-list original-buffer-undo-list)
                   (set-buffer-modidifed-p modified)))))))

(defun batch-makeinfo ()
  "Emacs makeinfo in batch mode."
  (infohack-texi-format (car command-line-args-left)
			(car (cdr command-line-args-left)))
  (setq command-line-args-left nil))


(defun infohack-texi-format (file &optional addsuffix)
  (let ((auto-save-default nil)
	(find-file-run-dired nil)
	coding-system-for-write
	output-coding-system
	(error 0))
    (condition-case err
	(progn
	  (find-file file)
	  (setq buffer-read-only nil)
	  (buffer-disable-undo (current-buffer))
	  (if (boundp 'MULE)
	      (setq output-coding-system file-coding-system)
	    (setq coding-system-for-write buffer-file-coding-system))
	  ;; Remove ignored areas first.
	  (while (re-search-forward "^@ignore[\t\r ]*$" nil t)
	    (delete-region (match-beginning 0)
			   (if (re-search-forward
				"^@end[\t ]+ignore[\t\r ]*$" nil t)
			       (1+ (match-end 0))
			     (point-max))))
	  (infohack-remove-unsupported)
	  (goto-char (point-min))
	  ;; Add suffix if it is needed.
	  (when (and addsuffix
		     (re-search-forward "^@setfilename[\t ]+\\([^\t\n ]+\\)"
					nil t)
		     (not (string-match "\\.info$" (match-string 1))))
	    (insert ".info")
	    (goto-char (point-min)))
	  ;; process @include before updating node
	  ;; This might produce some problem if we use @lowersection or
	  ;; such.
	  (let ((input-directory default-directory)
		(texinfo-command-end))
	    (while (re-search-forward "^@include" nil t)
	      (setq texinfo-command-end (point))
	      (let ((filename (concat input-directory
				      (texinfo-parse-line-arg))))
		(re-search-backward "^@include")
		(delete-region (point) (save-excursion
					 (forward-line 1)
					 (point)))
		(message "Reading included file: %s" filename)
		(save-excursion
		  (save-restriction
		    (narrow-to-region
		     (point) (+ (point)
				(car (cdr (insert-file-contents filename)))))
		    (goto-char (point-min))
		    ;; Remove `@setfilename' line from included file,
		    ;; if any, so @setfilename command not duplicated.
		    (if (re-search-forward "^@setfilename"
					   (save-excursion
					     (forward-line 100)
					     (point))
					   t)
			(progn
			  (beginning-of-line)
			  (delete-region (point) (save-excursion
						   (forward-line 1)
						   (point))))))))))
	  (texinfo-mode)
	  (texinfo-every-node-update)
	  (set-buffer-modified-p nil)
	  (message "texinfo formatting %s..." file)
	  (texinfo-format-buffer nil)
	  (if (buffer-modified-p)
	      (progn (message "Saving modified %s" (buffer-file-name))
		     (save-buffer))))
      (error
       (message ">> Error: %s" (prin1-to-string err))
       (message ">>  point at")
       (let ((s (buffer-substring (point) (min (+ (point) 100) (point-max))))
	     (tem 0))
	 (while (setq tem (string-match "\n+" s tem))
	   (setq s (concat (substring s 0 (match-beginning 0))
			   "\n>>  "
			   (substring s (match-end 0)))
		 tem (1+ tem)))
	 (message ">>  %s" s))
       (setq error 1)))
    (kill-emacs error)))

;;; infohack.el ends here
