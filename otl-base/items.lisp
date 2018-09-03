(in-package :otlb)
;;
;; items.lisp: access and manipulate members of object sequences
;;
(defun get-item-id (item)
  (second (get-item-spec item)))

(defun get-item-key (item)
  (first (get-item-spec item)))

(defun get-item-item-kvs (item)
  (item-spec-kvs (get-item-spec item)))

(defun get-item-obj-seq (item)
  "Return the object sequence associated with item ITEM."
  (second item))

(defun get-item-spec (item)
  (first item))

(defun item-kvs-val (key item-kvs)
  "Return value associated with one of the key/value pairs in the list ITEM-KVS." 
  (getf item-kvs key))

(defun item-spec-kvs (item-spec) 
  (if (> (length item-spec) 2)
      (subseq item-spec 2)))

(defun item-spec-kvs-val (key item-spec)
  (getf (item-spec-kvs item-spec) key))

(defun item-spec-p (item-spec?)
  (and
   (consp item-spec?)
   (keywordp (car item-spec?))))

(defun item-tablevel (item)
  (item-kvs-val :tablevel (get-item-item-kvs item)))

(defun item-tablevel-p (item tablevel)
  (= tablevel
     (item-kvs-val :tablevel (get-item-item-kvs item))))

;; FIXME: doc/internal-representation/items.txt indicates an item can be nil -- but the definition below doesn't allow that...
(defun itemp (item? &optional type)
  (and
   (consp item?)
   (item-spec-p (get-item-spec item?))
   (if type
       (equal (caar item?) type)
       t)))

(deftype item ()
  '(satisfies itemp))

;; generate an ITEM-ID value from an object sequence
;; - include some non-character top-level items in ID: indexterms, bracketed, glossterms
;; - exclude all other top-level items
(defun obj-seq->item-id-string (obj-seq)
  (with-output-to-string (s) 
    (dolist (item obj-seq) 
      (cond ((characterp item)
	     (write-char item s))
	    ((member (get-item-key item) '(:indexterm :bracket :glossterm))
	     (dolist (item (get-item-obj-seq item))
	       (if (characterp item) (write-char item s))))))))
(defun obj-seq->item-id-char-seq (obj-seq)
  (let ((l (length obj-seq))
	(char-seq nil))
    (do ((x (1- l) (1- x)))
	((< x 0))
      (let ((item (nth x obj-seq)))
	(cond ((characterp item)
	       (push item char-seq))
	      ((member (get-item-key item) '(:indexterm :bracket :glossterm))
	       (let ((item-obj-seq (get-item-obj-seq item)))
		 (let ((l-item-os (length item-obj-seq)))
		   (do ((y (1- l-item-os) (1- y)))
		       ((< y 0))
		     (let ((item2 (nth y item-obj-seq)))
		       (if (characterp item2) 
			   (push item2 char-seq)
			   )))))))))
    char-seq))

(defun set-item-key (item x)
  (assert (consp item))
  (assert (consp (first item)))
  (setf (first (first item)) x))

(defun set-item-id (item x)
  (assert (consp item))
  (assert (consp (first item)))
  (if (> 1 (length (first item)))
      (setf (second (first item)) x)
      (setf (first item) (cons (first (first item)) (list x)))))

(defun set-item-spec-id (item-spec x)
  (setf (second item-spec) x)
  item-spec)