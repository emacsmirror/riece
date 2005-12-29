;;; riece-cache.el --- LRU cache
;; Copyright (C) 1998-2005 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
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

;;; Code:

(defun riece-cache-make-node (key value &optional previous next)
  "Make riece-cache-node object."
  (vector key value previous next))

(defun riece-cache-node-key (node)
  "Return key of NODE."
  (aref node 0))

(defun riece-cache-node-value (node)
  "Return value of NODE."
  (aref node 1))

(defun riece-cache-node-set-value (node value)
  "Set value of NODE to VALUE."
  (aset node 1 value))

(defun riece-cache-node-next (node)
  "Return next of NODE."
  (aref node 3))

(defun riece-cache-node-set-next (node next)
  "Set next of NODE to NEXT."
  (aset node 3 next))

(defun riece-cache-node-previous (node)
  "Return previous of NODE."
  (aref node 2))

(defun riece-cache-node-set-previous (node previous)
  "Set previous of NODE to PREVIOUS."
  (aset node 2 previous))

(defun riece-make-cache (max-length)
  "Make riece-cache object."
  (vector max-length (make-vector (* max-length 2) 0) 0 nil nil))

(defun riece-cache-max-length (cache)
  "Return max-length of CACHE."
  (aref cache 0))

(defun riece-cache-hash-obarray (cache)
  "Return hash-obarray of CACHE."
  (aref cache 1))

(defun riece-cache-hash-length (cache)
  "Return hash-length of CACHE."
  (aref cache 2))

(defun riece-cache-set-hash-length (cache hash-length)
  "Set hash-length of CACHE to HASH-LENGTH."
  (aset cache 2 hash-length))

(defun riece-cache-first (cache)
  "Return first of CACHE."
  (aref cache 3))

(defun riece-cache-set-first (cache first)
  "Set first of CACHE to FIRST."
  (aset cache 3 first))

(defun riece-cache-last (cache)
  "Return last of CACHE."
  (aref cache 4))

(defun riece-cache-set-last (cache last)
  "Set last of CACHE to LAST."
  (aset cache 4 last))

(defun riece-cache-contains (cache key)
  "Return t if CACHE contains an entry whose key is KEY."
  (intern-soft key (riece-cache-hash-obarray cache)))

(defun riece-cache-get (cache key)
  "Return the value associated with KEY in CACHE.
If KEY is not associated in CACHE, it returns nil."
  (let ((node (riece-cache-get-node cache key)))
    (if node
	(riece-cache-node-value node))))

(defun riece-cache-get-node (cache key)
  "Return a node object associcated with KEY in CACHE.
If KEY is not associated in CACHE, it returns nil."
  (let ((symbol (intern-soft key (riece-cache-hash-obarray cache)))
	previous next last node)
    (when symbol
      (setq node (symbol-value symbol)
	    previous (riece-cache-node-previous node)
	    next (riece-cache-node-next node)
	    last (riece-cache-last cache))
      (if previous
	  (riece-cache-node-set-next previous next))
      (if next
	  (riece-cache-node-set-previous next previous))
      (riece-cache-node-set-next node nil)
      (riece-cache-node-set-previous node last)
      (riece-cache-node-set-next last node)
      (riece-cache-set-last cache node)
      (if (and (eq node (riece-cache-first cache)) next)
	  (riece-cache-set-first cache next))
      node)))

(defun riece-cache-delete (cache key)
  "Remove an entry from CACHE whose key is KEY."
  (let ((symbol (intern-soft key (riece-cache-hash-obarray cache)))
	previous next node)
    (when symbol
      (setq node (symbol-value symbol)
	    previous (riece-cache-node-previous node)
	    next (riece-cache-node-next node))
      (if previous
	  (riece-cache-node-set-next previous next))
      (if next
	  (riece-cache-node-set-previous next previous))
      (if (eq (riece-cache-last cache) node)
	  (riece-cache-set-last cache previous))
      (if (eq (riece-cache-first cache) node)
	  (riece-cache-set-first cache next))
      (unintern symbol (riece-cache-hash-obarray cache))
      (riece-cache-set-hash-length cache
				   (1- (riece-cache-hash-length cache)))
      (riece-cache-node-value node))))

(defun riece-cache-set (cache key value)
  "Associate KEY with VALUE in CACHE."
  (let ((node (riece-cache-get-node cache key)))
    (if node
	(riece-cache-node-set-value node value)
      (if (>= (riece-cache-hash-length cache)
	      (riece-cache-max-length cache))
	  (riece-cache-delete cache (riece-cache-node-key
				     (riece-cache-first cache))))
      (setq node (riece-cache-make-node key value (riece-cache-last cache)))
      (set (intern key (riece-cache-hash-obarray cache)) node)
      (riece-cache-set-hash-length cache
				   (1+ (riece-cache-hash-length cache)))
      (unless (riece-cache-first cache)
	(riece-cache-set-first cache node))
      (when (riece-cache-last cache)
	(riece-cache-node-set-next (riece-cache-last cache) node)
	(riece-cache-node-set-previous node (riece-cache-last cache)))
      (riece-cache-set-last cache node))))

(provide 'riece-cache)

;;; riece-cache.el ends here
