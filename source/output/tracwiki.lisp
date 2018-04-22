;;; Copyright 2014 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package :ccldoc)

(defun ccldoc:output-tracwiki (doc filename &key external-format (if-exists :supersede))
  (with-open-file (s filename :direction :output :if-exists if-exists :external-format external-format)
    (format s "[[PageOutline]]~2%")
    (write-tracwiki doc s)
    (truename s)))

(defmethod write-tracwiki ((clause cons) stream)
  (loop for sub in clause do (write-tracwiki sub stream)))

(defparameter *special-anywhere*
  '("''" "`" "//" "**" "{{{" "}}}" "::" "[" "]" "\\" "||"))

(defparameter *special-at-start-of-line* '("*" "=" "1." "a." ">"))

(defvar *wiki-indent*)
(defvar *nowiki-context*) ;; :single or :triple.
(defvar *wiki-inline*)

(defun wiki-freshline (stream)
  (fresh-line stream)
  (unless *nowiki-context*
    (dotimes (i *wiki-indent*) (write-char #\space stream)))
  (setq *wiki-inline* nil))

(defun wiki-newline (stream)
  (write-char #\Newline stream)
  (wiki-freshline stream))

(defun wiki-blankline (stream)
  (wiki-freshline stream)
  (wiki-newline stream))

(defun call-with-markup (start-text end-text stream fn)
  (ecase *nowiki-context*
    ((:triple)     ;; There is just no way to escape this.
     (funcall fn stream))
    ((:single)
     (write-string "`" stream)
     (let ((*nowiki-context* nil))
       (call-with-markup start-text end-text stream fn))
     (write-string "`" stream))
    ((nil)
     (write-string start-text stream)
     (funcall fn stream)
     (write-string end-text stream)))
  (setq *wiki-inline* t))

(defmacro with-markup ((stream-var start-text end-text) &body body)
  `(call-with-markup ,start-text ,end-text ,stream-var (lambda (,stream-var) ,@body)))

(defun write-nowiki (type clause stream)
  (if *nowiki-context*
    (write-tracwiki clause stream)
    (let ((*nowiki-context* type))
      (write-string (if (eq type :single) "`" "{{{") stream)
      (write-tracwiki clause stream)
      (write-string (if (eq type :single) "`" "}}}") stream))))


;; Output a basic text string, re-indenting all lines, quoting what needs to be quoted.
(defmethod write-tracwiki ((clause string) stream)
  (if *nowiki-context*
    (multiple-value-bind (end escape) (ecase *nowiki-context*
                                        (:single (values "`" "`{{{`}}}`"))
                                        (:triple (values "}}}" "}}}`}}}`{{{")))
      (loop
        for start = 0 then (+ pos (length end))
        for pos = (search end clause :start2 start)
        do (write-string clause stream :start start :end pos)
        while pos
        do (write-string escape stream)))
    (flet ((specials (line start esc specials)
             (loop for str in specials
               when (starts-with-subseq str line :start2 start)
               return (progn
                        (write-string esc stream)
                        (write-string str stream)
                        (length str))))
           (word (line start)
             (let ((end (or (position-if-not #'alpha-char-p line :start start) (length line))))
               (when (< start end)
                 ;; Quote if likely to be camelcase.
                 (when (and (<= (+ start 4) end)
                            (upper-case-p (char line start))
                            (lower-case-p (char line (1+ start)))
                            (find-if #'upper-case-p line :start (1+ start) :end end))
                   (write-char #\! stream))
                 (write-string line stream :start start :end end)
                 (- end start)))))
      (let ((lines (split-sequence #\newline clause)))
        (loop for line in lines as first = t then nil
          do (unless first (wiki-newline stream))
          do (let ((start (if *wiki-inline* 0 (position-if-not #'whitespacep line)))
                    (end (length line)))
                (when start
                  (when-let (skip (specials line start "``" *special-at-start-of-line*))
                    (incf start skip))
                  (loop while (< start end)
                    do (if-let (skip (or (specials line start "!" *special-anywhere*)
                                         (word line start)))
                         (incf start skip)
                         (progn
                           (write-char (char line start) stream)
                           (incf start))))
                  (setq *wiki-inline* t))))))))

(defmethod write-tracwiki ((clause docerror) stream)
  (with-markup (stream "**" "**")
    (write-tracwiki (clause-text clause) stream))
  (setq *wiki-inline* t))

(defmethod write-tracwiki ((clause document) stream)
  (let ((*nowiki-context* nil)
        (*wiki-indent* 0)
        (*wiki-inline* nil))
    (write-string "= " stream)
    (write-tracwiki (clause-title clause) stream)
    (write-string " =" stream)
    (wiki-newline stream)
    (write-tracwiki (clause-body clause) stream)
    (terpri stream)))

(defmethod write-tracwiki ((clause section) stream)
  (assert (not *nowiki-context*))
  (wiki-freshline stream)
  (if (typep (ancestor-of-type clause 'named-clause) 'definition)
    (write-tracwiki (clause-body clause) stream)
    (let ((level (min (1+ (section-level clause)) 6)))
      (write-string "======" stream :end level)
      (write-string " " stream)
      (write-tracwiki (clause-title clause) stream)
      (write-string " " stream)
      (write-string "======" stream :end level)
      (unless (typep clause 'document)
        (write-string " #" stream)
        (write-string (clause-external-id clause) stream))
      (wiki-newline stream)
      (write-tracwiki (clause-body clause) stream)))
  (terpri stream))

(defmethod write-tracwiki ((clause index-section) stream)
  (declare (ignore stream))
  (warn "Ignoring index section"))

(defmethod write-tracwiki ((clause indexed-clause) stream)
  (write-tracwiki (clause-body clause) stream))

(defmethod write-tracwiki ((clause glossary-section) stream)
  (declare (ignore stream))
  (warn "Ignoring glossary section"))

(defmethod write-tracwiki ((clause glossentry) stream)
  (declare (ignore stream))
  (error "Glossary entry outside glossary section??"))

(defmethod write-tracwiki ((clause block) stream)
  (cassert (not *nowiki-context*))
  (let ((*wiki-indent* (+ *wiki-indent* 1)))
    (wiki-freshline stream)
    (when-let (title (clause-title clause))
      (with-markup (stream "**" "**")
        (write-tracwiki title stream))
      (wiki-blankline stream))
    (write-tracwiki (clause-body clause) stream))
  (terpri stream))

(defmethod write-tracwiki ((clause code-block) stream)
  (cassert (not *nowiki-context*))
  (terpri stream)
  (write-nowiki :triple (clause-body clause) stream)
  (terpri stream))

(defmethod write-tracwiki ((clause para) stream)
   ;; might have to handle this cause, since we just turn blank lines into paragraphs.
  (cassert (not *nowiki-context*))
  (write-tracwiki (clause-body clause) stream)
  (wiki-blankline stream))

(defmethod write-tracwiki ((clause markup) stream)
  (ecase (markup-type clause)
    (:emphasis (with-markup (stream "//" "//")
                 (write-tracwiki (clause-body clause) stream)))
    ((:sample :param)
     (with-markup (stream "//" "//")
       (write-nowiki :single (clause-body clause) stream)))
    ((:system :code)
     (write-nowiki :single (clause-body clause) stream)))
  (setq *wiki-inline* t))

(defmethod write-tracwiki ((clause table) stream)
  (cassert (not *nowiki-context*))
  (loop for row across (clause-items clause) for i upfrom 0
    do (wiki-freshline stream)
    do (write-tracwiki row stream))
  (terpri stream))

(defmethod write-tracwiki ((clause row) stream)
  (cassert (not *nowiki-context*))
  (let ((headp (eq clause (aref (clause-items (clause-parent clause)) 0))))
    (wiki-freshline stream)
    (write-string "||" stream)
    (loop for item across (clause-items clause)
      do (when item
           (when headp (write-string "= " stream))
           (write-tracwiki item stream)
           (when headp (write-string " =" stream)))
      do (write-string "||" stream))
    (terpri stream)))

(defmethod write-tracwiki ((clause link) stream)
  (format stream "[~a " (link-url clause))
  (write-tracwiki (clause-body clause) stream)
  (format stream "]")
  (setq *wiki-inline* t))

(defmethod write-tracwiki ((clause listing) stream)
  (cassert (not *nowiki-context*))
  ;; Unfortunately tracwiki doesn't seem to be able to nest lists inside blockquotes,
  ;; so this comes out looking sucky in definitions.
  (let* ((type (listing-type clause))
         (prefix (ecase type
                   (:bullet "* ")
                   ;; In theory could use different prefix depending on level,
                   ;; but who really cares?
                   (:number "1. ")
                   ((:column :definition) " "))))
    (loop for item across (clause-items clause)
      do (wiki-freshline stream)
      do (when prefix (write-string prefix stream))
      do (let ((*wiki-indent* (+ *wiki-indent* (length prefix))))
           (write-tracwiki item stream))
      do (when (eq type :column)
           (write-string "[[BR]]" stream)))
    (terpri stream)
    (terpri stream)
    (wiki-blankline stream)))

(defmethod write-tracwiki ((clause term-item) stream)
  (cassert (not *nowiki-context*))
  (wiki-freshline stream)
  (write-tracwiki (clause-term clause) stream)
  (write-string ":: " stream)
  (let ((*wiki-indent* (+ *wiki-indent* 2)))
    (write-tracwiki (clause-body clause) stream))
  (setq *wiki-inline* t))

(defmethod write-tracwiki ((clause item) stream)
  (cassert (not *nowiki-context*))
  (write-tracwiki (clause-body clause) stream)
  (setq *wiki-inline* t))

(defmethod write-tracwiki ((clause definition) stream)
  (cassert (not *nowiki-context*))
  (wiki-freshline stream)
  (format stream "[=#~a] " (clause-external-id clause))
  (format stream "~%{{{
#!html
<tt><font size=\"+1\"><b>")
  (s-xml:print-string-xml (clause-text (definition-signature clause)) stream)
  (format stream "     </b></font><i>[")
  (s-xml:print-string-xml (dspec-type-name (clause-name clause)) stream)
  (format stream "]</i></tt>
}}}")
  (wiki-blankline stream)
  (when (definition-summary clause)
    (write-tracwiki (definition-summary clause) stream)
    (wiki-blankline stream))
  (write-tracwiki (clause-body clause) stream)
  (setq *wiki-inline* nil)
  (terpri stream))

(defmethod write-tracwiki ((clause xref) stream)
  ;; tracwiki doesn't allow formatting within link.
  ;; And it doesn't allow links within nowiki sections, 
  (with-markup (stream (format nil "[#~a " (clause-external-id (xref-target clause))) "]")
    (write-tracwiki (clause-text (or (clause-body clause) (xref-default-body clause))) stream))
  (setq *wiki-inline* t))
