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


#|
(progn
  (hemlock-interface:defindent "document" 1)
  (hemlock-interface:defindent "chapter" 1)
  (hemlock-interface:defindent "defsection" 1)
  (hemlock-interface:defindent "glosssary-section" 1)
  (hemlock-interface:defindent "text-block" 1)
  (hemlock-interface:defindent "listing" 1)
  (hemlock-interface:defindent "table" 2)
)
|#


(defun ccldoc:output-ccldoc (object filename &key external-format (if-exists :supersede))
  ;; We don't actually require OBJECT to be a DOCUMENT, to allow writing out separate include files...
  (let ((form (decompile-ccldoc object)))
    (with-open-file (s filename :direction :output :if-exists if-exists :external-format external-format)
      (pprint-ccldoc form s))))

(defmethod decompile-ccldoc ((clause clause-object))
  clause)

(defmethod decompile-ccldoc ((string string))
  (if (or (find #\{ string) (find #\} string))
    `(,(op-name :quote) ,string)
    string))

(defmethod decompile-ccldoc ((list list))
  `(clause ,@(mapcar #'decompile-ccldoc list)))

(defmethod decompile-ccldoc ((clause clause-with-body))
  `(,(default-operator clause) ,@(decompile-body clause)))

(defmethod decompile-body ((clause clause-with-body))
  (let ((body (clause-body clause)))
    (if (listp body)
      (mapcar #'decompile-ccldoc body)
      (list (decompile-ccldoc body)))))

(defmethod decompile-items ((clause clause-with-items))
  (map 'list #'decompile-ccldoc (clause-items clause)))

(defmethod decompile-ccldoc ((section section))
  (let ((package (slot-value section 'package)))
    `(,(default-operator section) (,(clause-title section) ,@(and package `(:package ,package))) ,@(decompile-body section))))

(defmethod decompile-ccldoc ((blk text-block))
  `(,(default-operator blk) (,(clause-title blk)) ,@(decompile-body blk)))

(defmethod decompile-ccldoc ((definition definition))
  `(,(default-operator definition) (,(dspec-type (clause-name definition))
                                  ,(dspec-name (clause-name definition)))
    ,(decompile-ccldoc (slot-value definition 'signature))
    ,(decompile-ccldoc (slot-value definition 'summary))
    ,@(decompile-body definition)))

(defmethod decompile-ccldoc ((glossentry glossentry))
  `(,(default-operator glossentry) ,(decompile-ccldoc (clause-term glossentry)) ,@(decompile-body glossentry)))

(defmethod decompile-ccldoc ((markup markup))
  `(,(default-operator markup) ,(markup-type markup) ,@(decompile-body markup)))

(defmethod decompile-ccldoc ((listing listing))
  `(,(default-operator listing) ,(listing-type listing) ,@(decompile-items listing)))

(defmethod decompile-ccldoc ((table table))
  `(,(default-operator table) (,(clause-title table)) ,@(decompile-items table)))

(defmethod decompile-ccldoc ((clause term-item))
  `(,(default-operator clause)
    ,(decompile-ccldoc (clause-term clause)) => ,@(decompile-body clause)))


(defmethod decompile-ccldoc ((clause reference-placeholder))
  `(,(default-operator clause)
    ,(decompile-clause-name (placeholder-target-name clause))
    ,@(decompile-body clause)))

(defmethod decompile-ccldoc ((clause xref))
  `(,(default-operator clause) 
    ,(decompile-clause-name (clause-name (xref-target clause)))
    ,@(decompile-body clause)))



(defmethod decompile-ccldoc ((row row))
  `(,(default-operator row) ,@(decompile-items row)))

(defun decompile-clause-name (name)
  (cond ((dspecp name)
         `(definition ,(if (eq (dspec-type name) t) (op-name :*) (dspec-type name)) ,(dspec-name name)))
        ((stringp name)
         `(glossentry ,name))
        ((null name) :document)
        (t (let* ((first (car name))
                  (op (if (table-name-p name) 'table 'section))
                  (title (if (eq op 'table) (cadr first) first)))
             `(,op ,title ,@(unless (op-name-p (cdr name) :*)
                              (let ((parent (decompile-clause-name (cdr name))))
                                (if (and (consp parent) (eq (car parent) 'section))
                                  `(:in ,@(cdr parent))
                                  `(:in ,parent)))))))))

(defmethod decompile-ccldoc ((link link))
  `(,(default-operator link) ,(link-url link) ,@(decompile-body link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pretty printer

(defparameter *ccldoc-pprint-dispatch* (copy-pprint-dispatch))

(defun pprint-ccldoc (form stream &key (package *package*))
  (let ((*package* (find-package :cl-user))
        (*print-pprint-dispatch* *ccldoc-pprint-dispatch*)
        (*print-case* :downcase)
        (*print-right-margin* 120))
    (pprint (prettify-ccldoc form :package package) stream)))

;; Don't print any package prefixes on operator symbols.
(defun operator-sym-p (sym) (not (null (ccldoc-opinfo sym))))
(defun pprint-ccldoc-operator (stream sym)
  (let ((*package* (symbol-package sym)))
    (print-object sym stream)))

(defun pprint-ccldoc-table (xp list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~^~W~^ ~:I~@_~W~^ ~:_~W~^~1I~@{ ~_~W~^~}~:>") xp list))

(flet ((set-pprint (type fn)
         (set-pprint-dispatch type fn 0 *ccldoc-pprint-dispatch*)))

  (loop for sym in '(document chapter defsection glossary-section text-block listing)
    do (set-pprint `(cons (member ,sym)) #'block-like))

  (loop for sym in '(row)
    do (set-pprint `(cons (member ,sym)) #'progn-print))

  (loop for sym in '(table)
    do (set-pprint `(cons (member ,sym)) #'pprint-ccldoc-table))

  (set-pprint '(satisfies operator-sym-p) #'pprint-ccldoc-operator))

;;; TODO: incorporate this info into the ccldoc definitions.
;; Offset to first evaluated arg.
(defparameter *form-subforms-start*
  '((:document 1)
    (:chapter 1)
    (:section 1)
    (:defsection 1)
    (:glossary-section 1)
    (:para 0)  ;; (:text)
    (:text-block 1)
    (:definition 1) ;; 
    (:glossentry 0)
    (:system 0)    ;; (:text)
    (:sample 0)    ;; (:text)
    (:param 0)    ;; (:text)
    (:code 0)    ;; (:text)
    (:code-block 0)
    (:emphasis 0)    ;; (:text)
    (:listing 1) ;; elts  ;; (:lisp :text)
    (:row 0) ;; elts  ;; (:text)
    (:table 1) ;; elts
    (:clause 0)
    (:item 0)
    (:term-clause 0) ;; .... => ...
    (:refdef 1000) ;; (:lisp :lisp)
    (:variable 1000)  ;; (:lisp)
    (:function 1000)  ;; (:lisp)
    (:type 1000)      ;; (:lisp)
    (:include-file 1000)
    (:index 0)
    (:index-section 1000)
    (:link 1)  ;; (:identifier :text) (def-operator link (url &rest subforms)
    (:markup 1)
    (:quote 0)
    (:term 0) ;; (:string) - only if no subforms  (def-operator term (gloss-target &rest subforms)
    (:ref 1)))	 ;; (:lisp :string)       (def-operator ref target &rest subforms)


(defun form-subforms-start (form)
  (when (consp form)
    (let ((offset (cadr (assoc (op-name (car form)) *form-subforms-start*)))
          (max (length form)))
      (cassert offset "Unknown form ~s" form)
      (min (1+ offset) max))))

(defun stringyp (x) (or (stringp x) (gensymp x)))

(defun gensymcat (&rest args)
  (let ((str (apply #'concatenate 'string (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) args))))
    (if (find #\" str) ;(find #\space str)
      (make-symbol str)
      str)))


(defun combine-strings (list)
  (flet ((parap (x)
           (and (consp x) (op-name-p (car x) :para) (stringyp (cadr x)) (null (cddr x))))
         (depara (para)
           ;; If paragraph comes from parsing something like <para>First line ..., then the first line
           ;; will have no indentation even though the rest of the paragraph might.  Make our first line
           ;; line up with the next one.
           (let* ((arg (cadr para))
                  (str (desym arg))
                  (eol (or (position #\Newline str)
                           (return-from depara arg)))
                  (next (or (position-if-not #'whitespacep str :start eol)
                            (return-from depara arg)))
                  (lpos (position #\Newline str :start eol :end next :from-end t)))
             (when (whitespacep (char str 0))
               (return-from depara arg))
             (gensymcat (subseq str (1+ lpos) next) str))))
    (loop with result = nil
      for form in list
      do (cond ((and (stringyp form) (stringyp (car result)))
                (setf (car result) (gensymcat (car result) form)))
               ((and (parap form) (or (stringyp (car result)) (parap (car result))))
                (cassert (or (consp (car result))
                             ;;Actually, must have a blank line, but too tough to check.
                             (find #\Newline (string (car result)))))
                (setf (car result) (gensymcat (if (consp (car result)) (cadr (car result)) (car result))
                                              '(#\Newline #\Newline)
                                              (depara form))))
               (t (push form result)))
      finally (return (nreverse result)))))

(defun prettify-ccldoc (form &key (package *package*))
  (if (not (eq package *package*))
    (let ((*package* (pkg-arg package)))
      (prettify-ccldoc form))
    (cond ((stringp form) (gensymcat form))
          ((non-nil-symbolp form) form)
          ((null form) nil)
          ((not (consp form)) (warn "Weird form ~s" form) form)
          (t (let* ((*print-case* :downcase)
                    (index (or (form-subforms-start form) (error "huh")))
                    (initial (subseq form 0 index))
                    (op (op-name (car initial)))
                    (subforms (mapcar #'prettify-ccldoc (subseq form index)))
                    (final (case op
                             ((:definition) (list* (car subforms) ;; synposis
                                                   (cadr subforms) ;; summary
                                                   (combine-strings (cddr subforms))))
                             ((:glossentry :term-item)
                              (cons (car subforms) ;; term
                                    (combine-strings (cdr subforms))))
                             ((:listing :row :table) subforms) ;; items
                             (t (combine-strings subforms))))
                    (pretty-form (append initial final)))
               (cond ((and (memq op '(:system :sample :param :code :emphasis)) ;; (:text)
                           (stringyp (car final))
                           (null (cdr final)))
                      (cassert (null (cdr initial)))
                      (gensymcat "{" (princ-to-string op) " " (car final) "}"))
                     ((memq op '(:variable :function :type :macro)) ;; (:lisp)
                      (cassert (null final))
                      (cassert (and (cdr initial) (null (cddr initial))))
                      (let* ((text (prin1-to-string (cadr initial))))
                        (if (or (find #\{ text) (find #\} text))
                          pretty-form
                          (gensymcat "{" (princ-to-string op) " " text "}"))))
                     ((eq op :refdef) ;; (:lisp :lisp)
                      (cassert (null final))
                      (cassert (and (cddr initial) (null (cdddr initial))))
                      (let* ((type (prin1-to-string (cadr initial)))
                             (text (prin1-to-string (caddr initial))))
                        (if (or (find #\{ text) (find #\} text) (find #\{ type) (find #\} type))
                          pretty-form
                          (gensymcat "{" (princ-to-string op) " " type " " text "}"))))
                     ((and (memq op '(:chapter :section))
                           (stringyp (cadr initial))
                           (evenp (length final))
                           (loop for (in ttl) on final by #'cddr
                             always (and (op-name-p in :in) (stringyp ttl))))
                      (gensymcat "{"  (princ-to-string op)
                                 " "
                                 (format nil "~{~a~^::~}"
                                         (nreverse (cons (cadr initial)
                                                         (loop for ttl in (cdr final) by #'cddr collect ttl))))
                                 "}"))
                     ((and (eq op :link)  ;; (:identifier :text)
                           (stringyp (cadr initial))
                           (not (find-if #'whitespacep (cadr initial)))
                           (or (null final)
                               (and (stringyp (car final))
                                    (null (cdr final)))))
                      (cassert (null (cddr initial)))
                      (if (null final)
                        (gensymcat "{" (princ-to-string op) " " (cadr initial) "}")
                        (gensymcat "{" (princ-to-string op) " " (cadr initial) " " (car final) "}")))
                     ((and (eq op :term) ;; (:string)
                           (null final)
                           (stringyp (cadr initial)))
                      (cassert (null (cddr initial)))
                      (gensymcat "{" (princ-to-string op) " " (cadr initial) "}"))
                     (t pretty-form)))))))


