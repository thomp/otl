(in-package :otlp)
;;
;; parse-sub.lisp: implement substitution table
;;

;; a sample substitution table using unicode characters as implemented by SBCL
(defparameter *subst-table*
  '(
    ;; mathematical symbols
    ("+/-"  . ((:NOOP) (#\PLUS-MINUS_SIGN)))
    ;; scientific units
    ("oC " . ((:NOOP) (#\DEGREE_CELSIUS #\SPACE)))
    ("oC," . ((:NOOP) (#\DEGREE_CELSIUS #\,)))
    ("oC." . ((:NOOP) (#\DEGREE_CELSIUS #\.)))
    ("oC:" . ((:NOOP) (#\DEGREE_CELSIUS #\:)))
    ("oC;" . ((:NOOP) (#\DEGREE_CELSIUS #\;)))
    ("oC)" . ((:NOOP) (#\DEGREE_CELSIUS #\))))
    ("oC?" . ((:NOOP) (#\DEGREE_CELSIUS #\?)))
    ("/ug" . ((:NOOP) (#\/ #\GREEK_SMALL_LETTER_MU #\g)))
    (" ug" . ((:NOOP) (#\Space #\GREEK_SMALL_LETTER_MU #\g)))
    (" uL" . ((:NOOP) (#\Space #\GREEK_SMALL_LETTER_MU #\L)))
    ("/uL" . ((:NOOP) (#\/ #\GREEK_SMALL_LETTER_MU #\L)))
    ("umol" . ((:NOOP) (#\GREEK_SMALL_LETTER_MU #\m #\o #\l)))
    ;; biochemistry/chemistry
    ("Ca2+" . ((:NOOP) (#\C #\a ((:SUP) (#\2 #\+)))))
    ("Ca++" . ((:NOOP) (#\C #\a ((:SUP) (#\+ #\+)))))
    ("CH3" . ((:NOOP) (#\C #\H ((:SUB) (#\3)))))
    ("Cl2" . ((:NOOP) (#\C #\l ((:SUB) (#\2)))))
    ("CO2" . ((:NOOP) (#\C #\O ((:SUB) (#\2)))))
    ;; ("Cu2\\+" ("Cu\\textsuperscript{2+}")) 
    ("Eo\\S" . ((:NOOP) (#\E ((:SUP) (#\o)))))
    ;; ("Eo'\\S"		"E\\textsubscript{o}'")
    ("Fe2+" . ((:NOOP) (#\F #\e ((:SUP) (#\2 #\+)))))
    ("Fe3+" . ((:NOOP) (#\F #\e ((:SUP) (#\3 #\+)))))
    ("H+" . ((:NOOP) (#\H ((:SUP) (#\+)))))
    ("h+" . ((:NOOP) (#\h ((:SUP) (#\+)))))
    ("H2PO4" . ((:NOOP) (#\H ((:SUB) (#\2)) #\P #\O ((:SUB) (#\4)))))
    ("H2" . ((:NOOP) (#\H ((:SUB) (#\2)))))
    ("H3O+" . ((:NOOP) (#\H ((:SUB) (#\3)) #\O ((:SUP) (#\+)))))
    ("H3" . ((:NOOP) (#\H ((:SUB) (#\3)))))
    ("HPO4" . ((:NOOP) (#\H #\P #\O ((:SUB) (#\4)))))
    ("Keq" . ((:NOOP) (#\K ((:SUB) (#\e #\q)))))
    ("Mg2+" . ((:NOOP) (#\M #\g ((:SUP) (#\2 #\+)))))
    ("Mg++" . ((:NOOP) (#\M #\g ((:SUP) (#\+ #\+)))))
    ("NAD+" . ((:NOOP) (#\N #\A #\D ((:SUP) (#\+)))))
    ("Na+" . ((:NOOP) (#\N #\a ((:SUP) (#\+)))))
    ("Na2" . ((:NOOP) (#\N #\a ((:SUB) (#\2)))))
    ("Na3" . ((:NOOP) (#\N #\a ((:SUB) (#\3)))))
    ("NaN3" . ((:NOOP) (#\N #\a #\N ((:SUB) (#\3)))))
    ("O2" . ((:NOOP) (#\O ((:SUB) (#\2)))))
    ("OH-" . ((:NOOP) (#\O #\H ((:SUP) (#\-)))))
    ("pKa" . ((:NOOP) (#\p #\K ((:SUB) (#\a)))))
    ;; (" sp3"		" sp\\textsuperscript{3}")
    ;; (" sp2"		" sp\\textsuperscript{2}")
    ;; (" t1/2"		" t\\textsubscript{1/2}") 
    )
  "An alist where keys are strings and values are otl items")

;; these are used due to limitation of CHOICES (see below)
(defparameter *subst-table-match-strings*
  (mapcar #'(lambda (pair)
	      (car pair))
	  *subst-table*))

(defparameter *subst-table-otl-targets*
  (mapcar #'(lambda (pair)
	      (cdr pair))
	  *subst-table*))

(defun subst? ()
  (pc:mdo (pc:<- match (choices-list *subst-table-match-strings*))
       (pc:result
	;; CHOICES doesn't provide a means for knowing which match occurred so...
	(nth (position match *subst-table-match-strings* :test #'string=)
	     *subst-table-otl-targets*))))
