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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CCLDOC DOM
;;
;; Basic ccldoc type is CLAUSE
;; A clause is a CLAUSE-OBJECT, or a STRING, or a LIST of clause-objects or strings.
;;   A STRING represents leaf data
;;   A LIST (of clause-objects or strings - no embedded lists, they get flattened out) represents a sequence of text

;; compilation state variables.
(defvar *parent-clause*)
(defvar *current-file*)
(defvar *source-form*)
(defvar *section-package*)

(defclass clause-object ()
  ((parent :type (or null clause-object) :initform *parent-clause* :accessor clause-parent)
   (source-form :initform *source-form* :reader clause-source-form)))

;; For decompiling.
(defmethod default-operator ((clause clause-object))
  (op-name (class-name (class-of clause))))

(deftype clause () '(or string clause-object cons))

(defclass inline-clause (clause-object) ())

(defgeneric inline-clause-p (clause)
  (:method ((clause string)) t)
  (:method ((list list)) (every #'inline-clause-p list))
  (:method ((clause inline-clause)) t)
  (:method ((clause clause-object)) nil))

(defmethod print-object ((c clause-object) stream)
  (if *print-escape*
    (print-unreadable-object (c stream :identity t :type t)
      (princ-ccldoc c stream))
    (princ-ccldoc c stream)))

(defmethod princ-ccldoc ((c clause-object) stream)
  (let ((text (clause-text c)))
    (when (> (length text) 50)
      (setq text (concatenate 'string (subseq text 0 50) "...")))
    (setq text (substitute #\nko_digit_one #\newline text))
    (princ text stream)))

;; Could automate this, but for now.
(defgeneric subclause-slots (clause) (:method-combination append))
(defmethod subclause-slots append ((clause clause-object)) nil)
(defmethod subclause-slots append ((clause string)) nil)

(defun ancestor-of-type (clause-object type)
  (loop for parent = (clause-parent clause-object) then (clause-parent parent)
    when (or (null parent) (typep parent type)) return parent))

(defmethod clause-parent ((list cons))
  (error "Bug: Shouldn't call clause-parent on a list"))

(defmethod (setf clause-parent) (parent (list cons))
  (loop for subclause in list do (setf (clause-parent subclause) parent))
  parent)

(defmethod clause-parent ((string string))
  (error "Bug: shouldn't call clause-parent on a string"))

(defmethod (setf clause-parent) (parent (string string))
  parent)


(defmethod clause-section ((clause clause-object))
  (ancestor-of-type clause 'section))

(defmethod clause-document ((clause clause-object))
  (ancestor-of-type clause 'document))

(defmethod section-level ((clause clause-object))
  (section-level (clause-parent clause)))

(defmethod clause-text ((clause string)) clause)

(defmethod clause-text ((clause clause-object)) "")


(defmethod clause-text ((clauses list))
  (reduce (lambda (text clause) (concatenate 'string text (clause-text clause))) clauses :initial-value ""))

(defclass docerror (clause-object)
  ((clause-text :initarg :message :reader clause-text)))

;; named clauses have a globally unique name (a lisp object, compared with EQUALP) which can be used to
;;  reference them from other objects.  The name is only used during compilation, once the DOM is built,
;;  the references have been turned into pointers and the names should not be needed.
(defclass named-clause (clause-object)
  (;; Lisp object naming this clause
   (name :initform nil :initarg :name :accessor clause-name)
   ;; Unique id string for this clause, suitable for use in html or xml.
   (external-id :type string :accessor clause-external-id)))

;; Don't use an after method because subclasses need to initialize name in their own after methods
(defmethod initialize-instance :around ((clause named-clause) &key)
  (call-next-method)
  (let* ((name (clause-name clause))
         (doc (clause-document clause))
         (old (gethash name (named-clauses doc))))
    (cassert (not old) "Duplicate clause name ~s: ~s and ~s" name (type-of old) (type-of clause))
    (setf (gethash name (named-clauses doc)) clause)))

(defclass clause-with-title (clause-object)
  ((title :initarg :title :reader clause-title)))

(defmethod clause-text ((clause clause-with-title))
  (concat-by #\Newline (clause-title clause) (call-next-method)))

(defclass clause-with-required-title (named-clause clause-with-title)
  ((title :type string)))

(defmethod clause-reference-name ((clause clause-with-required-title)) (clause-title clause))

(defclass clause-with-optional-title (clause-with-title)
  ((title :initform nil :type (or null string))))

(defclass clause-with-body (clause-object)
  ((body :type (or null clause) :accessor clause-body)))
(defmethod subclause-slots append ((clause clause-with-body)) '(body))

(defmethod clause-text ((clause clause-with-body))
  (clause-text (clause-body clause)))

(defclass clause-with-required-body (clause-with-body)
  ((body :type clause)))

(defclass clause-with-term (clause-object)
  ((term :initarg :term :type (or null clause) :reader clause-term)))  
(defmethod subclause-slots append ((clause clause-with-term)) '(term))

;; This is a clause with items -- a sequence of clauses where the position matters,
;; so they can't be arbitrarily combined the way BODY can.
(defclass clause-with-items (clause-object)
  ((items :type vector :reader clause-items)))
(defmethod subclause-slots append ((clause clause-with-items)) '(items))

(defmethod clause-text ((clause clause-with-items))
  (loop with result = ""
    for item across (slot-value clause 'items)
    do (setq result (concatenate 'string result (clause-text item) '(#\Newline)))
    finally (return result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Concrete classes

(defclass item (clause-with-body)
  ())

(defclass term-item (item clause-with-term clause-with-body)
  ((default-operator :allocation :class :initform (op-name :item) :reader default-operator)))

(defmethod clause-text ((clause term-item))
  (concatenate 'string (clause-text (clause-term clause)) " - " (call-next-method)))


(defclass para (clause-with-body)
  ())

(defmethod clause-text ((clause para))
  ;; Close enough.  This is just for debugging anyway.
  (concatenate 'string (call-next-method) (string #\Newline)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package (find-package :cl)))

;;; TODO change this to a different name - defclassing CL:BLOCK is undefined behaviour
(defclass block (clause-with-optional-title clause-with-body)
  ())

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package (find-package :cl)))

(deftype markup-type () '(member :emphasis :system :sample :param :code))

(defclass markup (clause-with-body inline-clause)
  ((markup-type :type markup-type :initarg :type :reader markup-type)))

;; Should this just be a type of markup?
(defclass code-block (clause-with-body)
  ())

(deftype listing-type () '(member :bullet :number :column :definition))

;; each item is a clause-with-body, except for definition, where it is a term-clause
(defclass listing (clause-with-items)
  ((listing-type :type listing-type :initarg :type :reader listing-type)))

(defclass row (clause-with-items)
  ())

(defmethod clause-text ((clause row))
  (loop with result = ""
    for item across (slot-value clause 'items)
    do (setq result (concatenate 'string result (clause-text item) '(#\Tab)))
    finally (let ((len (length result)))
              (return (if (eql len 0) result (subseq result 0 (1- len)))))))

(defclass table (clause-with-required-title clause-with-items)
  ())


(defmethod initialize-instance :after ((clause table) &key)
  (let ((parent (ancestor-of-type clause 'named-clause)))
    (setf (clause-name clause) (cons (list :table (clause-title clause)) (clause-name parent)))))


(defclass definition (named-clause clause-with-body)
  ((display-name :type clause :accessor definition-display-name)
   (signature :type clause :accessor definition-signature)
   (summary :type (or null clause) :accessor definition-summary)))
(defmethod subclause-slots append ((clause definition)) '(display-name signature summary))

(defmethod clause-reference-name ((clause definition)) (definition-display-name clause))

(defmethod clause-text ((clause definition))
  (concat-by #\Space
             (clause-text (definition-display-name clause))
             (clause-text (definition-signature clause))
             (clause-text (definition-summary clause))
             (call-next-method)))

(defclass glossentry (named-clause clause-with-term clause-with-body)
  ())

(defmethod clause-text ((clause glossentry))
  (concatenate 'string (clause-text (clause-term clause)) " - " (call-next-method)))

(defmethod clause-reference-name ((clause glossentry)) (clause-term clause))

(defclass link (clause-with-required-body inline-clause)
  ((url :type string :initarg :url :reader link-url)))

;; Text to add to index, which will reference back to here.
;; (index-entry "foo bar") will add "foo bar" to index and make it point to here.
(defclass indexed-clause (clause-with-body inline-clause)
  ((default-operator :allocation :class :initform (op-name :index) :reader default-operator)))
(defmethod subclause-slots append ((clause indexed-clause)) '(default-body))


;; clause with link to target.
(defclass xref (indexed-clause)
  ((default-operator :allocation :class :initform (op-name :ref) :reader default-operator)
   (default-body :initarg :default-body :reader xref-default-body)
   (target :type named-clause :initarg :target :reader xref-target)))

;; These shouldn't exist except during compilation, to be replaced by an indexed-clause (plain or xref).
(defclass reference-placeholder (indexed-clause)
  ((default-operator :allocation :class :initform (op-name :ref) :reader default-operator)
   (default-body :initarg :default-body :reader clause-default-body)
   (target-name :initarg :name :reader placeholder-target-name)))

(defclass section (clause-with-required-title clause-with-body)
  ((default-operator :allocation :class :initform (op-name :defsection) :reader default-operator)
  ;; TODO: This slot is only used for debugging, specifically decompile-ccldoc...
   (package :initform nil :initarg :package)))

(defmethod initialize-instance :after ((clause section) &key)
  (let ((parent (ancestor-of-type clause 'named-clause)))
    (setf (clause-name clause) (and parent ;; nil for toplevel document
                                    (cons (clause-title clause) (clause-name parent))))))


(defmethod section-level ((clause section))
  (let ((parent (ancestor-of-type clause 'section)))
    (if parent (1+ (section-level parent)) 0)))

(defclass glossary-section (section)
  ())

(defun glossary-section (doc)
  (find-clause doc (lambda (clause) (typep clause 'glossary-section)) nil))

(defclass index-section (section)
  ((body :initform nil :type null)))

(defun index-section (doc)
  (find-clause doc (lambda (clause) (typep clause 'index-section)) nil))

(defclass document (section)
  ((parent :type null :initform nil)
   (named-clauses :initform (make-hash-table :test #'equalp) :reader named-clauses)
   (external-ids :initform (make-hash-table :test #'equal) :reader external-ids)))

(defmethod clause-document ((clause document))
  (cassert (null (clause-parent clause)))
  clause)

(defun named-clauses-count (doc) (hash-table-count (named-clauses doc)))

(defun matching-clauses (doc fn)
  (loop for clause being the hash-value of (named-clauses doc) when (funcall fn clause) collect clause))

(defun find-clause (doc fn &optional (errorp t))
  (let ((matches (matching-clauses doc fn)))
    (cassert (null (cdr matches)) "Multiple clauses matching ~s in ~s" fn doc)
    (cassert (or matches (not errorp)) "There is no clause matching ~s in ~s" fn doc)
    (car matches)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  named clause name protocol.


;; A canonical name either a fully qualified clause name, or a wildcard name:
;;   - NIL ;; the toplevel document.
;;   - DSPEC ;; definition clause name, possibly wild.
;;   - STRING ;; glossentry clause name
;;   - (:index . nil) or (:glossary . nil) ;; special section name
;;   - (string . parent-canonical-name) ;; section clause name.
;;   - (string . *) ;; wildcard section clause name
;;   - ((:table string) . parent-canonical-name) ;; table clause name
;;   - ((:table string) . *) ;; wildcard table clause name
(defun canonical-clause-name-p (name)
  (or (null name)
      (dspecp name)
      (stringp name)
      (equal name '(:index))
      (equal name '(:glossary))
      (and (consp name)
           (or (stringp (car name))
               (and (consp (car name))
                    (eq (caar name) :table)
                    (string (cadar name))
                    (null (cddar name))))
           (or (op-name-p (cdr name) :*) ;; relative section name
               (canonical-clause-name-p (cdr name))))))

(defun section-name-p (name)
  (and (not (dspecp name)) (not (stringp name))))

(defun table-name-p (name)
  (and (consp name)
       (consp (car name))
       (eq (caar name) :table)))

;; Compute the fully-qualified version of name - used to look for exact match before dwimming
(defun as-fully-qualified-name (doc name)
  #+gz (cassert (canonical-clause-name-p name))
  (cond ((or (null name)
             (dspecp name)
             (stringp name))
         name)
        ((equal name '(:index))
         (and (index-section doc)
              (clause-name (index-section doc))))
        ((equal name '(:glossary))
         (and (glossary-section doc)
              (clause-name (glossary-section doc))))
        (t
         (cons (car name) (if (op-name-p (cdr name) :*)
                            nil
                            (as-fully-qualified-name doc (cdr name)))))))

(defun wildcard-canonical-name-p (cname)
  (or (dspecp cname) ;; dspec always wild because of subclassing
      (and (consp cname)
           (not (keywordp (car cname)))
           (or (op-name-p (cdr cname) :*)
               (wildcard-canonical-name-p (cdr cname))))))

(defun wild-name-match-p (clause-name wild-name)
  (if (consp clause-name)
    (and (consp wild-name)
         (equalp (car clause-name) (car wild-name))
         (or (op-name-p (cdr wild-name) :*)
             (wild-name-match-p (cdr clause-name) (cdr wild-name))))
    (and (dspecp clause-name)
         (dspecp wild-name)
         (let ((type (dspec-type clause-name)))
           (dspec-subtypep type (dspec-type wild-name))
           (equalp (dspec-name clause-name)
                   (if (wild-dspec-p wild-name)
                     (canonicalize-definition-name type (dspec-name wild-name))
                     (dspec-name wild-name)))))))


(defun resolve-placeholder (clause)
  (let* ((doc (clause-document clause))
         (name (placeholder-target-name clause))
         (exact-match (let ((clause-name (as-fully-qualified-name doc name)))
                        (and clause-name (gethash clause-name (named-clauses doc)))))
         (matches (and (not exact-match)
                       (wildcard-canonical-name-p name)
                       (matching-clauses doc (lambda (clause) (wild-name-match-p (clause-name clause) name)))))
         (target (or exact-match (car matches))))
    (cond ((cdr matches)
           (warn "Ambiguous reference ~s: ~s" name matches)
           (change-class clause 'docerror :message (format nil "Ambiguous reference ~s: ~s" name matches)))
          (target
           (change-class clause 'xref :target target))
          ((section-name-p name)
           (warn "Reference to unknown section: ~s" name)
           (change-class clause 'docerror :message (format nil "Reference to unknown section: ~s" name)))
          (t
           ;; TODO: need some verbosity control for this warning.  It should be off by default, since it's the
           ;; advertised behavior, but sometimes might want to turn it on to check for missing entries.
           (if (dspecp name)
             ;; For now, just warn about stuff that doesn't exist in the runtime, or has multiple matches, as
             ;; those cases are more likely to be user errors, as opposed to just simply an undocumented definition.
             (let ((defined (case (dspec-type name)
                              ((:macro :generic-function :function) (fboundp (dspec-name name)))
                              (:variable (ccl::proclaimed-special-p (dspec-name name)))
                              (:type (and (symbolp (dspec-name name))
                                          (ccl::get-type-predicate (dspec-name name))))))
                   (matches (matching-clauses doc (lambda (clause)
                                                    (and (typep clause 'definition)
                                                         (equalp (dspec-name (clause-name clause)) (dspec-name name)))))))
               (if matches
                 (warn "Unresolved reference to ~a ~s, perhaps should be ~{~a~^ or ~}"
                       (dspec-type name) (dspec-name name) (mapcar (lambda (c) (dspec-type (clause-name c))) matches))
                 (unless defined
                   (warn "Unresolved reference to undefined ~a ~s" (dspec-type name) (dspec-name name)))))
             (warn "Unresolved reference to ~s changed to index entry" name))
           (unless (clause-body clause)
             (setf (clause-body clause) (clause-default-body clause)))
           (change-class clause 'indexed-clause)))))

(defun resolve-xref-targets (doc)
  (labels ((resolve (thing)
             (cond ((null thing) nil)
                   ((typep thing '(or cons (vector t)))
                    (map nil #'resolve thing))
                   (t
                    (loop for slot in (subclause-slots thing)
                      do (resolve (slot-value thing slot)))
                    (when (typep thing 'reference-placeholder)
                      (resolve-placeholder thing))))))
    (resolve doc)))


