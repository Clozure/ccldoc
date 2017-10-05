;;;   Copyright (C) 2014 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html
(in-package :ccldoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operator and expander definitions

(def-expander document (title-and-options &rest sections)
  (cassert (null *parent-clause*))
  `(defsection ,title-and-options ,@sections))

;; Used both as a chapter definition and as a reference to a chapter, dependng on context.
(def-expander chapter (title-and-options &rest sections)
  :parser-types (:string)
  (if (and (typep *parent-clause* 'section)
           (eql (section-level *parent-clause*) 0))
    (progn
      `(defsection ,title-and-options ,@sections))
    (progn
      (cassert (null sections) "No arguments allowed in chapter reference: ~s" sections)
      `(ref (chapter ,title-and-options)))))

(def-expander section (title-and-options &rest parent-spec)
  :parser-types (:string)
  `(ref (section ,title-and-options ,@parent-spec)))

(def-expander variable (symbol)
  :parser-types (:lisp)
  `(ref (definition variable ,symbol)))

(def-expander function (fspec)
  :parser-types (:lisp)
  `(ref (definition function ,fspec)))

(def-expander macro (fspec)
  :parser-types (:lisp)
  `(ref (definition macro ,fspec)))

(def-expander class (symbol)
  :parser-types (:lisp)
  `(ref (definition class ,symbol)))


(def-expander type (typespec)
  :parser-types (:lisp)
  `(ref (definition type ,typespec)))

;;; TODO: perhaps should have just name, optional type...
(def-expander refdef (type name)
  :parser-types (:lisp :lisp)
  ;; type/name name a definition.  It should name a definition, but if it doesn't, gets turned into an indexed clause
  `(ref (definition ,type ,name)))

;; Evaluate but ignore markup in title.  This is so can use macros like {CCL} that expand into text.
(defun normalize-title (title)
  (let ((title-clause (form-clause-ensuring-simple title)))
    (when title-clause (clause-text title-clause))))

(def-expander term (target)
  :parser-types (:string)
  ;; target is a string naming a text phrase. It should name a glossentry, but if it
  ;; doesn't, it gets turned into an indexed clause.
  `(ref (glossentry ,(require-type (normalize-title target) 'string))))

(def-expander emphasis (&rest subforms)
  :parser-types (:text)
  `(markup :emphasis ,@subforms))

(def-expander system (&rest subforms)
  :parser-types (:text)
  `(markup :system ,@subforms))

;; A sample value, to be replaced by actual value (as opposed to parameter, which is a name to be replace by actual value)
(def-expander sample (&rest subforms) ;; was replaceable
  :parser-types (:text)
  `(markup :sample ,@subforms))

(def-expander param (&rest subforms)
  :parser-types (:text)
  `(markup :param ,@subforms))

(def-expander code (&rest subforms)
  :parser-types (:text)
  `(markup :code ,@subforms))

(def-expander lbrace ()
  :parser-types ()
  `(quote "{"))

(def-expander rbrace ()
  :parser-types ()
  `(quote "}"))

;; Include files can be any number of lisp forms, followed by any number ccldoc forms.
(def-operator include-file (filename &key in-package (external-format :default))
  (let* ((*current-file* (merge-pathnames filename (or *current-file* *default-pathname-defaults*)))
         (*package* (if in-package (pkg-arg in-package) *package*))
         (forms (with-ccldoc-packages
                    (with-open-file (stream *current-file*
                                            :element-type 'base-char
                                            :external-format
                                            #-ccl (or external-format :utf-8)
                                            #+ccl (if (eq external-format :default) :inferred external-format))
                      (loop for form = (load-to-ccldoc-form filename stream) then (read stream nil stream)
                        until (eq form stream) collect form)))))
    (subforms-clause *parent-clause* forms)))

;; Quoted string means doesn't contain any ccldoc { markup }
;;  TODO: should we allow arbitrary sexp's and replace with prin1-to-string?
(def-operator quote (string)
  (cassert (stringp string))
  string)

(defun parse-arg-and-options (arg-and-options &key (options nil) (type t))
  (unless (listp arg-and-options)
    (setq arg-and-options (list arg-and-options)))
  (let* ((arg (if (listp arg-and-options) (car arg-and-options) arg-and-options))
         (specified-options (if (listp arg-and-options) (cdr arg-and-options) nil))
         (bad (loop for key in specified-options by #'cddr unless (member key options) collect key))
         (option-values (loop for key in options collect (getf specified-options key))))
    (cassert (typep arg type) "Argument ~s is not of type ~s" arg type)
    (cassert (not bad) "Invalid options ~s" bad)
    (values-list (cons arg option-values))))

;; TODO: only sections allowed inside CHAPTER.
(def-operator defsection (title-and-options &rest subforms)
  (multiple-value-bind (title package) (parse-arg-and-options title-and-options :type 'string :options '(:package))
    (let* ((clause (make-instance (if *parent-clause* 'section 'document) :title (normalize-title title) :package package))
           (*section-package* (if package
                                (or (find-package package) (ccldoc-package package))
                                *section-package*)))
      (para-subforms-to-body clause subforms))))

(def-operator glossary-section (title-and-options &rest subforms)
  (cassert (eql (section-level *parent-clause*) 0) "Glossary must be at chapter level")
  (multiple-value-bind (title) (parse-arg-and-options title-and-options :type 'string :options ())
    (let ((sect (make-instance 'glossary-section :title (normalize-title title))))
      (para-subforms-to-body sect subforms)
      ;; there should be nothing in the glossary except the entries, which are handled separately.
      (cassert (null (clause-body sect)))
      sect)))

(defun collect-glossary-entries (doc)
  (let* ((sect (glossary-section doc))
         (entries (matching-clauses doc (lambda (clause) (typep clause 'glossentry)))))
    (if (not sect)
      (cassert (null entries) "Missing glossary section")
      (when entries
        (setf (clause-body sect) (if (cdr entries)
                                   (sort entries #'string-lessp :key #'clause-name)
                                   (car entries)))))))

;; TODO: get rid of this and add :index-title option to book.
(def-operator index-section (title-and-options)
  (cassert (eql (section-level *parent-clause*) 0) "Index must be at chapter level")
  (multiple-value-bind (title) (parse-arg-and-options title-and-options :type 'string :options ())
    (make-instance 'index-section :title (normalize-title title))))

(def-operator para (&rest subforms)
  :parser-types (:text)
  (subforms-to-body (make-instance 'para) subforms))

;; Like a subsection but doesn't get numbered and doesn't generate a target
(def-operator block (title-and-options &rest subforms)
  (multiple-value-bind (title) (parse-arg-and-options title-and-options
                                                      :type '(or null string)
                                                      :options ())
    (para-subforms-to-body (make-instance 'block :title (normalize-title title)) subforms)))

(def-operator code-block (&rest subforms)
  (subforms-to-body (make-instance 'code-block) subforms))

;; Documentation for a definition.
(def-operator definition ((type name)
                        signature
                        summary
                        &rest subforms)
  (let* ((dspec (make-dspec type (desym name)))
         (name-form (text-for-clause-name dspec))
         (definition (make-instance 'definition :name dspec)))
    (let ((*parent-clause* definition))
      (setf (slot-value definition 'display-name) (form-clause name-form))
      ;; TODO: do something more dwimmy here...
      (setf (slot-value definition 'signature) (form-clause (or signature name-form)))
      ;; 86 definitions have a summary, 449 don't.
      (setf (slot-value definition 'summary) (and summary (form-clause summary)))
      (let* ((body (subforms-clause definition subforms t))
             (list (if (listp body) body (list body))))
        (setf (slot-value definition 'body)
              (if (some (lambda (x) (typep x 'section)) list)
                body
                (let ((section (make-instance 'section :title "Description")))
                  (loop for clause in list do (setf (clause-parent clause) section))
                  (setf (slot-value section 'body) body)
                  section)))))
    definition))

(def-operator glossentry (term &rest subforms)
  ;; Build TERM with *PARENT-CLAUSE* being wrong, because we need to process it first
  ;; so can compute glossentry name from the term.  Most of the time term is a string, and in any
  ;; case it mustn't include any named clauses or anything that cares about parentage during creation.
  (let ((*section-package* (find-package :cl-user)))
    (let* ((term-clause (form-clause-ensuring-simple term))
           (name (normalize-whitespace (clause-text term-clause)))
           (glossentry (make-instance 'glossentry :term term-clause :name name)))
      (setf (clause-parent term-clause) glossentry) ;; TODO: perhaps initialize should do this.
      (para-subforms-to-body glossentry subforms)))
  ;; don't record the entry in its parent, will appear in glossary
  nil)

(def-operator markup (type &rest subforms)
  :parser-types (:lisp :text)
  (subforms-to-body (make-instance 'markup :type type) subforms))

(def-operator listing (type-and-options &rest forms)
  :parser-types (:lisp :text)
  (multiple-value-bind (type) (parse-arg-and-options type-and-options :type 'listing-type :options ())
    ;; TODO: each item needs to have inlined forms combined.
    (subforms-to-items (make-instance 'listing :type type)
                       forms
                       (if (eq type :definition) 'term-item 'item))))

(def-operator row (&rest items)
  :parser-types (:text)
  (cassert (typep *parent-clause* 'table))
  (subforms-to-items (make-instance 'row) items))

(def-operator table (title-and-options head-row &rest rows)
  (multiple-value-bind (title) (parse-arg-and-options title-and-options :type 'string :options ())
    (subforms-to-items (make-instance 'table :title (normalize-title title)) (cons head-row rows) 'row)))

;; like progn, makes a single clause from multiple clauses;
(def-operator clause (&rest forms)
  (subforms-clause *parent-clause* forms))

(def-operator item (&rest forms)
  (let* ((parent *parent-clause*)
         (split (position-if #'(lambda (x) (op-name-p x :=>)) forms)))
    (cassert (typep parent 'clause-with-items))
    (if (and (typep parent 'listing) (eq (listing-type parent) :definition))
      (let ((term-forms (subseq forms 0 split))
            (defn-forms (and split (subseq forms (1+ split))))
            (item (make-instance 'term-item)))
        (setf (slot-value item 'term) (subforms-clause item term-forms))
        (para-subforms-to-body item defn-forms))
      (let ((item (make-instance 'item)))
        (cassert (null split) "=> can only appear in definition listing items")
        (if (and (typep parent 'listing)
                 (not (memq (listing-type parent) '(:column))))
          (para-subforms-to-body item forms)
          (subforms-to-body item forms))))))

(def-operator index (&rest subforms)
  :parser-types (:text)
  (subforms-to-body (make-instance 'indexed-clause) subforms))

;;; section title can be "outer::less-outer...inner::innermost"  parent-spec is (:IN  parent :IN parent ...)
(def-operator ref (target-name &rest subforms)
  :parser-types (:lisp :text)
  (let* ((name (canonicalize-ref-name (desym target-name)))
         (ref (make-instance 'reference-placeholder :name name)))
    ;; Default body in case don't find a target.
    ;; Compute it now so it's processed in the right context.
    (setf (slot-value ref 'default-body)
          (unless (setf (clause-body ref) (subforms-clause ref subforms))
            (subforms-clause ref (list (text-for-clause-name name)))))
    ref))

(defun canonicalize-ref-name (name)
  (when (eq name :document) (setq name nil))
  (when (member name '(:index :glossary)) (setq name (list name)))
  (let ((op-name (and (consp name) (op-name (car name)))))
    (cond ((op-name-p op-name :definition)
           (destructuring-bind (type lisp-name) (cdr name)
             (if (op-name-p type :*)
               (make-wild-dspec lisp-name)
               (make-dspec type lisp-name))))
          ((op-name-p op-name :glossentry)
           (destructuring-bind (term) (cdr name)
             (normalize-title term)))
          ((or (op-name-p op-name :section)
               (op-name-p op-name :chapter)
               (op-name-p op-name :table))
           (destructuring-bind (title &rest parent-specs) (cdr name)
             (if (and (null parent-specs) (member title '(:index :glossary)))
               (list title)
               (let* ((title (normalize-title title))
                      (titles (nreconc (let ((split (loop for start = 0 then (+ pos 2)
                                                      for pos = (search "::" title :start2 start)
                                                      collect (and (< start (or pos (length title)))
                                                                   (subseq title start pos))
                                                      while pos)))
                                         (when (equal (car split) ".")
                                           (setf (car split) :document))
                                         split)
                                       (loop for (in parent) on parent-specs by #'cddr
                                         do (cassert (eq in :in))
                                         collect parent))))
                 (cassert (stringp (car titles)))
                 (cassert (every #'stringp (butlast titles)))
                 (cond ((op-name-p op-name :chapter)
                        (when (cdr titles)
                          (error "Invalid chapter name ~s" name)))
                       (t
                        (let ((final (car (last titles))))
                          (if (stringp final)
                            (nconc titles (op-name :*))
                            (nconc (butlast titles) (canonicalize-ref-name final))))
                        (when (op-name-p op-name :table)
                          (setf (car titles) (list (op-name :table) (car titles))))))
                 titles))))
          (t (if (canonical-clause-name-p name) name (error "Invalid name ~s" name))))))

(def-operator link (url &rest forms)
  :parser-types (:identifier :text)
  (cassert (stringp url))
  (let ((link (make-instance 'link :url url)))
    (setf (clause-body link) (or (subforms-clause link forms) url))
    link))

(defmethod para-subforms-to-body ((parent clause-with-body) forms)
  (setf (slot-value parent 'body) (subforms-clause parent forms t))
  parent)

(defmethod subforms-to-body ((parent clause-with-body) forms)
  (setf (slot-value parent 'body) (subforms-clause parent forms nil))
  parent)

(defmethod subforms-to-items ((parent clause-with-items) forms &optional (type 'item))
  (setf (slot-value parent 'items) (subforms-items parent forms type))
  parent)

(defun subforms-clause (parent forms &optional upgrade-to-para)
  (let* ((*parent-clause* parent)
         (clauses (loop for form in forms as clause = (form-clause form)
                    if (listp clause) append clause else collect clause)))
    (when upgrade-to-para
      (labels ((upgrade (list)
                 (when list
                   (let* ((n (position-if-not #'inline-clause-p list)))
                     (if (eql n 0)
                       (cons (car list) (upgrade (cdr list)))
                       (let ((group (subseq list 0 n))
                             (para (make-instance 'para))) ;; current parent
                         (loop for clause in group do (setf (clause-parent clause) para))
                         (setf (slot-value para 'body) (if (cdr group) group (car group)))
                         (cons para (and n (upgrade (nthcdr n list))))))))))
        (setq clauses (upgrade clauses))))
    (if (cdr clauses)
      clauses
      (car clauses))))

(defun subforms-items (parent forms type)
  (let* ((*parent-clause* parent)
         (items (loop for form in forms as item = (form-clause form)
                  do (cassert (or (null item) (typep item type) (typep item 'docerror)))
                  collect item)))
    (coerce items 'vector)))


(defun form-clause (form)
  (let ((*source-form* form))
    (when-let (expanded (ccldoc-macroexpand form))
      (handler-bind ((error (lambda (c)
                              (restart-case (error c)
                                (continue ()
                                  :report "Output error text into document and continue"
                                  (return-from form-clause
                                    (make-instance 'docerror :message (format nil "Error: ~a" c))))))))
        (form-clause-by-operator (car expanded) (cdr expanded))))))

;; Build the TERM with *parent-clause* being wrong, because it will be needed in processing
;; of the real parent.  Most of the time the term is a string, and in any case it mustn't
;; include any named clauses or anything that cares about parentage during creation.
(defun form-clause-ensuring-simple (term)
  (let* ((*parent-clause* nil))
    (form-clause term)))
