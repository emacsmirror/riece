(defun riece-lru-make-node (key value &optional previous next)
  "Make riece-lru-node object."
  (vector key value previous next))

(defun riece-lru-node-key (node)
  "Return key of NODE."
  (aref node 0))

(defun riece-lru-node-value (node)
  "Return value of NODE."
  (aref node 1))

(defun riece-lru-node-next (node)
  "Return next of NODE."
  (aref node 3))

(defun riece-lru-node-set-next (node next)
  "Set next of NODE to NEXT."
  (aset node 3 next))

(defun riece-lru-node-previous (node)
  "Return previous of NODE."
  (aref node 2))

(defun riece-lru-node-set-previous (node previous)
  "Set previous of NODE to PREVIOUS."
  (aset node 2 previous))

(defun riece-lru-make-map (max-length)
  "Make riece-lru-map object."
  (vector max-length (make-vector (* max-length 2) 0) 0 nil nil))

(defun riece-lru-map-max-length (map)
  "Return max-length of MAP."
  (aref map 0))

(defun riece-lru-map-hash-obarray (map)
  "Return hash-obarray of MAP."
  (aref map 1))

(defun riece-lru-map-hash-length (map)
  "Return hash-length of MAP."
  (aref map 2))

(defun riece-lru-map-set-hash-length (map hash-length)
  "Set hash-length of MAP to HASH-LENGTH."
  (aset map 2 hash-length))

(defun riece-lru-map-first (map)
  "Return first of MAP."
  (aref map 3))

(defun riece-lru-map-set-first (map first)
  "Set first of MAP to FIRST."
  (aset map 3 first))

(defun riece-lru-map-last (map)
  "Return last of MAP."
  (aref map 4))

(defun riece-lru-map-set-last (map last)
  "Set last of MAP to LAST."
  (aset map 4 last))

(defalias 'riece-make-lru 'riece-lru-make-map)

(defun riece-lru-contains (map key)
  (intern-soft key (riece-lru-map-hash-obarray map)))

(defun riece-lru-get (map key)
  (let ((node (riece-lru-get-node map key)))
    (if node
	(riece-lru-node-value node))))

(defun riece-lru-get-node (map key)
  (let ((symbol (intern-soft key (riece-lru-map-hash-obarray map)))
	previous next first last node)
    (when symbol
      (setq node (symbol-value symbol)
	    previous (riece-lru-node-previous node)
	    next (riece-lru-node-next node)
	    first (riece-lru-map-first map)
	    last (riece-lru-map-last map))
      (if previous
	  (riece-lru-node-set-next previous next))
      (if next
	  (riece-lru-node-set-previous next previous))
      (riece-lru-node-set-next node nil)
      (riece-lru-node-set-previous node last)
      (riece-lru-node-set-next last node)
      (riece-lru-map-set-last map node)
      (if (and (eq node (riece-lru-map-first map)) next)
	  (riece-lru-map-set-first map next))
      node)))

(defun riece-lru-delete (map key)
  (let ((symbol (intern-soft key (riece-lru-map-hash-obarray map)))
	previous next node)
    (when symbol
      (setq node (symbol-value symbol)
	    previous (riece-lru-node-previous node)
	    next (riece-lru-node-next node))
      (if previous
	  (riece-lru-node-set-next previous next))
      (if next
	  (riece-lru-node-set-previous next previous))
      (if (eq (riece-lru-map-last map) node)
	  (riece-lru-map-set-last map previous))
      (if (eq (riece-lru-map-first map) node)
	  (riece-lru-map-set-first map next))
      (unintern symbol (riece-lru-map-hash-obarray map))
      (riece-lru-map-set-hash-length map (1- (riece-lru-map-hash-length map)))
      (riece-lru-node-value node))))

(defun riece-lru-set (map key value)
  (let ((node (riece-lru-get-node map key))
	symbol)
    (if node
	(aset node 1 value)
      (if (>= (riece-lru-map-hash-length map)
	      (riece-lru-map-max-length map))
	  (riece-lru-delete map (riece-lru-node-key
				 (riece-lru-map-first map))))
      (setq node (riece-lru-make-node key value (riece-lru-map-last map) nil))
      (set (intern key (riece-lru-map-hash-obarray map)) node)
      (riece-lru-map-set-hash-length map (1+ (riece-lru-map-hash-length map)))
      (unless (riece-lru-map-first map)
	(riece-lru-map-set-first map node))
      (if (riece-lru-map-last map)
	  (progn
	    (riece-lru-node-set-next (riece-lru-map-last map) node)
	    (riece-lru-node-set-previous node (riece-lru-map-last map))))
      (riece-lru-map-set-last map node))))
