;;; lunit-report.el --- output test report in XML compatible with JUnitTask

;; Copyright (C) 2004 Daiki Ueno.

;; Author: Daiki Ueno <ueno@unixuser.org>

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

;;; Code:

(require 'lunit)

(eval-and-compile
  (luna-define-class lunit-test-reporter (lunit-test-listener)
		     (buffer
		      start-time))

  (luna-define-internal-accessors 'lunit-test-reporter))

;; stolen (and renamed) from time-date.el.
(defun lunit-time-since (time)
  "Return the time elapsed since TIME."
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 time))
		 (expt 2 16))))
    (list (- (+ (car current) (if rest -1 0)) (car time))
	  (- (+ (or rest 0) (nth 1 current)) (nth 1 time))
	  (- (nth 2 current) (nth 2 time)))))

(defun lunit-escape-quote (string)
  (let ((index 0))
    (while (string-match "\"" string index)
      (setq string (replace-match "&quot;" nil t string)
	    index (+ 5 index)))
    string))
    
(luna-define-method lunit-test-listener-error ((reporter lunit-test-reporter)
					       case error)
  (save-excursion
    (set-buffer (lunit-test-reporter-buffer-internal reporter))
    (insert (format "\
      <error message=\"%s\" type=\"error\"/>
"
		    (lunit-escape-quote (pp-to-string error))))))

(luna-define-method lunit-test-listener-failure ((reporter lunit-test-reporter)
						 case failure)
  (save-excursion
    (set-buffer (lunit-test-reporter-buffer-internal reporter))
    (insert (format "\
      <failure message=\"%s\" type=\"failure\"/>
"
		    (lunit-escape-quote (pp-to-string failure))))))

(luna-define-method lunit-test-listener-start ((reporter lunit-test-reporter)
					       case)
  (save-excursion
    (set-buffer (lunit-test-reporter-buffer-internal reporter))
    (goto-char (point-max))
    (narrow-to-region (point) (point))
    (insert (format "\
    <testcase name=\"%s\" classname=\"%s\">
"
		    (lunit-test-name-internal case)
		    (luna-class-name case)))
    (lunit-test-reporter-set-start-time-internal reporter (current-time))))

(luna-define-method lunit-test-listener-end ((reporter lunit-test-reporter)
					     case)
  (let ((elapsed
	 (lunit-time-since
	  (lunit-test-reporter-start-time-internal reporter))))
    (save-excursion
      (set-buffer (lunit-test-reporter-buffer-internal reporter))
    
      (insert "\
    </testcase>
")
      (goto-char (point-min))
      (looking-at " *<testcase\\>")
      (goto-char (match-end 0))
      (insert (format " time=\"%.03f\" "
		      (+ (nth 1 elapsed)
			 (/ (nth 2 elapsed) 1000000.0))))
      (widen))))

(defun lunit-report (test file)
  "Run TEST and output result as XML."
  (let* ((printer
	  (luna-make-entity 'lunit-test-printer))
	 (result
	  (lunit-make-test-result printer))
	 (buffer (find-file-noselect file))
	 start-time)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (lunit-test-result-add-listener
     result
     (luna-make-entity 'lunit-test-reporter :buffer buffer))
    (setq start-time (current-time))
    (lunit-test-run test result)
    (let ((assert-count
	   (lunit-test-result-assert-count-internal result))
	  (failures
	   (lunit-test-result-failures-internal result))
	  (errors
	   (lunit-test-result-errors-internal result))
	  
	  (elapsed (lunit-time-since start-time)))
      (princ (format "%d runs, %d assertions, %d failures, %d errors\n"
		     (lunit-test-number-of-tests test)
		     assert-count
		     (length failures)
		     (length errors)))
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(insert (format "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<testsuites>
  <testsuite name=\"\" tests=\"%d\" failures=\"%d\" \
errors=\"%d\" time =\"%.03f\">
    <properties>
      <property name=\"emacs-version\" value=\"%s\"/>
    </properties>
"
			(lunit-test-number-of-tests test)
			(length failures)
			(length errors)
			(+ (nth 1 elapsed)
			   (/ (nth 2 elapsed) 1000000.0))
			(lunit-escape-quote (emacs-version))))
	(goto-char (point-max))
	(insert "\
  </testsuite>
</testsuites>")
	(save-buffer)))))

(provide 'lunit-report)