;;; riece-cache.el --- LRU based cache management
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

(defun riece-cache-make-map (max-length)
  "Make riece-cache-map object."
  (vector max-length (make-vector (* max-length 2) 0) 0 nil nil))

(defun riece-cache-map-max-length (map)
  "Return max-length of MAP."
  (aref map 0))

(defun riece-cache-map-hash-obarray (map)
  "Return hash-obarray of MAP."
  (aref map 1))

(defun riece-cache-map-hash-length (map)
  "Return hash-length of MAP."
  (aref map 2))

(defun riece-cache-map-set-hash-length (map hash-length)
  "Set hash-length of MAP to HASH-LENGTH."
  (aset map 2 hash-length))

(defun riece-cache-map-first (map)
  "Return first of MAP."
  (aref map 3))

(defun riece-cache-map-set-first (map first)
  "Set first of MAP to FIRST."
  (aset map 3 first))

(defun riece-cache-map-last (map)
  "Return last of MAP."
  (aref map 4))

(defun riece-cache-map-set-last (map last)
  "Set last of MAP to LAST."
  (aset map 4 last))

(defalias 'riece-make-cache 'riece-cache-make-map)

(defun riece-cache-contains (map key)
  (intern-soft key (riece-cache-map-hash-obarray map)))

(defun riece-cache-get (map key)
  (let ((node (riece-cache-get-node map key)))
    (if node
	(riece-cache-node-value node))))

(defun riece-cache-get-node (map key)
  (let ((symbol (intern-soft key (riece-cache-map-hash-obarray map)))
	previous next last node)
    (when symbol
      (setq node (symbol-value symbol)
	    previous (riece-cache-node-previous node)
	    next (riece-cache-node-next node)
	    last (riece-cache-map-last map))
      (if previous
	  (riece-cache-node-set-next previous next))
      (if next
	  (riece-cache-node-set-previous next previous))
      (riece-cache-node-set-next node nil)
      (riece-cache-node-set-previous node last)
      (riece-cache-node-set-next last node)
      (riece-cache-map-set-last map node)
      (if (and (eq node (riece-cache-map-first map)) next)
	  (riece-cache-map-set-first map next))
      node)))

(defun riece-cache-delete (map key)
  (let ((symbol (intern-soft key (riece-cache-map-hash-obarray map)))
	previous next node)
    (when symbol
      (setq node (symbol-value symbol)
	    previous (riece-cache-node-previous node)
	    next (riece-cache-node-next node))
      (if previous
	  (riece-cache-node-set-next previous next))
      (if next
	  (riece-cache-node-set-previous next previous))
      (if (eq (riece-cache-map-last map) node)
	  (riece-cache-map-set-last map previous))
      (if (eq (riece-cache-map-first map) node)
	  (riece-cache-map-set-first map next))
      (unintern symbol (riece-cache-map-hash-obarray map))
      (riece-cache-map-set-hash-length map (1- (riece-cache-map-hash-length map)))
      (riece-cache-node-value node))))

(defun riece-cache-set (map key value)
  (let ((node (riece-cache-get-node map key)))
    (if node
	(riece-cache-node-set-value node value)
      (if (>= (riece-cache-map-hash-length map)
	      (riece-cache-map-max-length map))
	  (riece-cache-delete map (riece-cache-node-key
				 (riece-cache-map-first map))))
      (setq node (riece-cache-make-node key value (riece-cache-map-last map)))
      (set (intern key (riece-cache-map-hash-obarray map)) node)
      (riece-cache-map-set-hash-length map
				       (1+ (riece-cache-map-hash-length map)))
      (unless (riece-cache-map-first map)
	(riece-cache-map-set-first map node))
      (if (riece-cache-map-last map)
	  (progn
	    (riece-cache-node-set-next (riece-cache-map-last map) node)
	    (riece-cache-node-set-previous node (riece-cache-map-last map))))
      (riece-cache-map-set-last map node))))

(provide 'riece-cache)

;;; riece-cache.el ends here
