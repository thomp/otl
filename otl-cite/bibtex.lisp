(in-package :otl-cite)
;;
;; bibtex.lisp: entry and database handling via CL-BIBTEX
;;



;; 1. !! at start of run need to clear bibtex database and *bib-files* 
;;   (otl-cite::reset-cite)
;;   (setf otlr::*citations-encountered* nil)
;; 2. set up *BIB-DATABASE* as a hash table (see bibtex.lisp/BIBTEX-COMPILER:BIBTEX)
     (defparameter bibtex-compiler::*bib-database* (make-hash-table :test #'equalp)) 
;; 3. then LOAD-ENTRIES
;;   (otl-cite::load-entries "/home/thomp/academic/references/biochemistry-lab-manual-AUTO-KEYS")
;; 4. then parse and render
;;   (otl::parse-and-render-file "/home/thomp/fpu/classes/BIOL450--genetics/labs/reports/reports-short.txt" :html)



(defun load-entries (path)
  "Load BibTeX entries at path PATH into database/store." 
  (bibtex-runtime::setf bibtex-runtime:*bib-files* (list path))
  (bibtex-runtime::read-all-bib-files))

(defun get-entry (key)
  "Return entry with BibTeX key KEY."
  (bibtex-runtime::get-merged-bib-entry key))

(defun entry-author (entry)
  (gethash "author" entry))

(defun entry-journal (entry)
  (gethash "journal" entry))

(defun entry-title (entry)
  (gethash "title" entry))

(defun entry-year (entry)
  (gethash "year" entry))

(defun reset-cite ()
  (bibtex-runtime::setf bibtex-runtime:*bib-files* nil)
  (if (hash-table-p bibtex-runtime:*bib-database*)
      (clrhash bibtex-runtime:*bib-database*)))
