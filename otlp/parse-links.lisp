(in-package :otlp)
;;;
;;; parse links
;;;
(defparameter *container-id* nil "Stack for strings, each representing the id of an element.")

#|

Define a link target with [[foo]]
- 'foo' can't contain any ']' characters

Point at a link with <<foo,somelabel>>

|#
(defun linktarget? ()
  (pc:mdo 
    "[["
    (pc:<- linkid (pc:atleast?
		(pc:choices
		 (cons?)
		 (not-char? #\])) 1))
    "]]"
    (pc:result 
     (let ((linkid-string (otlb::tostring linkid)))
       ;; set id of container
       ;; FIXME: use a special lexical var %container-id% ?
       (push linkid-string *container-id*)
       (list (list :LINKTARGET linkid-string))))))

;; an internal link: <<someid,somelabel>> or <<someid>>
(defun linkpointer?-otl ()
  (pc:mdo 
    "<<"
    ;; the linkid can't be greedy (must exclude >>) and can't include a comma
    (pc:<- linkid 
	(pc:find-before? (pc:item) (pc:choices1 ">>" #\,)))
    (pc:<- linklabel
	(pc:atmost? (pc:mdo 
		      #\,
		      (pc:<- x (pc:find-before? 
			     (pc:choices
			      (cons?)
			      (pc:item)) 
			     ">>"))
		      (pc:result x))
		    1))
    ">>"
    (pc:result 
     ;; (:linkpointer id linkid linklabel) where linkid is a string and linklabel is an object sequence
     (progn
       (list (list :LINKPOINTER 
		   nil 
		   ;; linkid is id of element pointer points at 
		   :to-id (otlb::tostring linkid))
	     ;; if optional linklabel isn't specified, use id
	     (if linklabel 
		 ;; the ATMOST? (above) leaves linklabel with the form (object-seq) (a list holding an object sequence list)
		 (first linklabel) linkid))))))
 
