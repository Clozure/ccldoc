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

;; Loads a ccldoc file and returns a DOCUMENT object.
;; A ccldoc file should contain lisp forms followed by a single ccldoc DOCUMENT definition.
(defun ccldoc:load-document (filename &key (external-format :default))
  (require-type (build-ccldoc (read-ccldoc-document filename external-format) :default-file filename) 'document))

;; A ccldoc file should contain lisp forms followed by a single ccldoc DOCUMENT definition.
(defun read-ccldoc-document (filename external-format)
  (setq filename (merge-pathnames filename))
  (with-ccldoc-packages
      (with-open-file (stream filename
                              :element-type 'base-char
                              :external-format
                              #-ccl (or external-format :utf-8)
                              #+ccl (if (eq external-format :default) :inferred external-format))
        (prog1 (load-to-ccldoc-form filename stream)
          (unless (eq (read stream nil :eof) :eof)
            (error "Extranenous forms following document in ~s" filename))))))

(defun load-to-ccldoc-form (file stream)
  ;; Since we're allowing arbitrary lisp code to precede the document, set up a load-like environment
  ;; so can meta-. the definitions in the lisp part.
  (let* ((file (merge-pathnames file))
         (*readtable* *readtable*)
         (*loading-file-source-file* file)
         (*load-pathname* file)
         (*load-truename* (truename stream))
         (*nx-source-note-map* (and *save-source-locations* (make-hash-table :test #'eq)))
         (*loading-toplevel-location* nil)
         (form nil))
    (loop
      (multiple-value-setq (form *loading-toplevel-location*)
        (read-recording-source stream :eofval stream :file-name *loading-file-source-file*
                               :map *nx-source-note-map*
                               :save-source-text (neq *save-source-locations* :no-text)))
      (when (eq form stream)
        (error "No ccldoc form found in ccldoc file ~s" file))
      (when (and (consp form)
                 (symbolp (car form))
                 ;; Stop at first form that could be a ccldoc form.  They can wrap it in PROGN
                 ;; if they want it interpreted as a lisp form.
                 (ccldoc-opinfo (car form)))
        (return form))
      (cheap-eval-in-environment form nil))))

(defun build-ccldoc (doc-form &key default-file)
  (let ((*show-condition-context* nil)
        (doc (let* ((*parent-clause* nil)
                    (*current-file* (and default-file (merge-pathnames default-file))) ;; default for INCLUDE's.
                    (*section-package* (find-package :cl-user)))
               (form-clause doc-form))))
    (cassert (typep doc 'document))
    (collect-glossary-entries doc)
    (resolve-xref-targets doc)
    (assign-external-ids doc)
    doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; operators
;;;

(defvar *operators* (make-hash-table :test 'eq))

(defstruct (opinfo (:type list))
  type
  function
  parser)

(defun ccldoc-opinfo (name)
  (when-let (op-name (op-name name))
    (gethash op-name *operators*)))

(defun ccldoc-macro-expander (name)
  (when-let (opinfo (ccldoc-opinfo name))
    (and (eq (opinfo-type opinfo) :expander)
         (opinfo-function opinfo))))

(defun ccldoc-operator (name)
  (when-let (opinfo (ccldoc-opinfo name))
    (and (eq (opinfo-type opinfo) :operator)
         (opinfo-function opinfo))))

(defun ccldoc-string-parser (name)
  (when-let (opinfo (ccldoc-opinfo name))
    (let ((parser (opinfo-parser opinfo)))
      (cond ((listp parser)
             (apply #'make-arg-reader parser))
            ((eq parser :none)
             nil)
            (t parser)))))

(defun %def-operator (type op-name params fn docstring &key (parser-types (if (null params) nil :none)))
  (check-type type (member :expander :operator))
  (let ((op-name (op-name op-name t)))
    (record-source-file op-name 'ccldoc)
    (record-arglist op-name params)
    (setf (gethash op-name *operators*)
          (make-opinfo :type type
                       :function fn
                       :parser parser-types))
    (when docstring (setf (documentation op-name 'ccldoc) docstring))
    op-name))

(defmacro def-operator (name params &body body)
  (let* ((op-name (op-name name t))
         (options (loop while (keywordp (car body)) collect (pop body) collect `',(pop body)))
         (argsvar (gensym))
         (doc (and (stringp (car body)) (pop body)))
         (lambda-form `(lambda (,argsvar)
                         (destructuring-bind ,params ,argsvar
                           ,@body))))
    `(%def-operator :operator ',op-name ',params (nfunction ,op-name ,lambda-form) ,doc ,@options)))

(defmacro ccldoc:def-expander (name params &body body &environment env)
  (check-type name symbol)
  (let ((op-name (op-name name t))
        (options (loop while (keywordp (car body)) collect (pop body) collect `',(pop body))))
    (multiple-value-bind (lambda-form doc) (parse-macro-1 name params body env)
      `(%def-operator :expander ',op-name ',params (nfunction ,op-name ,lambda-form) ,doc ,@options))))


(defun form-clause-by-operator (operator args)
  (let ((fn (ccldoc-operator operator)))
    (if fn
      ;; Aha, now we could expand it
      (funcall fn args)
      (error "Unknown form ~s" (cons operator args)))))

(defun ccldoc-macroexpand (form)
  (loop
    (when (stringp (setq form (desym form)))
      (setq form (parse-ccldoc-string form)))
    (cassert (listp form))
    (let ((expander (ccldoc-macro-expander (car form))))
      (unless expander (return form))
      (setq form (funcall expander form nil)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings with embedded directives
;;; 
;;; ccldoc strings can be used anywhere a clause is allowed and they can can contain embedded directives.
;;; Embedded directives take the form {command args...}.  Args... may contain nested embedded directives.
;;; Curly braces are special in ccldoc strings and there is no way to escape or quote them.  If you need to
;;; reference a curly brace in a text string, use the lisp QUOTE syntax.  The command and any lisp args
;;; are read using the compile-time *package* and *readtable*.
;;; 

(defun string-chunk (string start &optional end)
  (let* ((max-end (+ start 50))
         (end (or end (length string))))
    (if (<= end max-end)
      (subseq string start end)
      (concatenate 'string (subseq string start max-end) "..."))))

(defun delim-pos (string start &optional end)
  (position-if (lambda (ch) (or (eql ch #\{) (eql ch #\}))) string :start start :end end))

(defun end-delim-pos (string start &optional end)
  (let ((opens 1))
    (loop
      (setq start (delim-pos string start end))
      (cond ((null start) (return nil))
            ((eql (aref string start) #\{) (incf opens))
            (t (when (eql (decf opens) 0) (return start))))
      (incf start))))

(defun lisp-from-string (string &key start end (eof nil eofp))
  (let* ((end (or end (delim-pos string start) (length string))))
    (multiple-value-bind (val pos) (let ((*read-eval* nil)) (read-from-string string (not eofp) eof :start start :end end))
      (loop while (and (< pos end) (whitespacep (char string pos))) do (incf pos))
      (values val pos))))

(defun parse-ccldoc-string (string)
  (labels ((parse (string start)
             (when (< start (length string))
               (let ((startpos (delim-pos string start)))
                 (if (null startpos)
                   `(quote ,(subseq string start))
                   (if (eql (char string startpos) #\})
                     (error "Stray close brace: ~s" (string-chunk string startpos))
                     (let* ((head (and (> startpos start) `(quote ,(subseq string start startpos))))
                            ;; Could allow unmatched braces in lisp args by deferring end-delim-pos til after read all the lisp args,
                            ;; but doesn't seem worth the complication...
                            (endpos (or (end-delim-pos string (1+ startpos))
                                        (error "Unclosed directive: ~s..." (string-chunk string startpos))))
                            (middle (multiple-value-bind (command nextpos) (lisp-from-string string :start (1+ startpos))
                                      (cassert (non-nil-symbolp command) "Invalid directive: ~s" (string-chunk string startpos nextpos))
                                      (let ((parser (or (ccldoc-string-parser command)
                                                        (error "Unknown directive ~s" command))))
                                        (funcall parser command string nextpos endpos))))
                            (tail (parse string (1+ endpos)))
                            (forms (nconc (and head (list head))
                                          (and middle (list middle))
                                          (and tail (if (and (consp tail) (eq (car tail) 'clause)) (cdr tail) (list tail))))))
                       (cond ((null forms) nil)
                             ((null (cdr forms)) (car forms))
                             (t `(clause ,@forms)))))))))
           (listify (x)
             (and x (if (and (consp x) (eq (car x) 'clause)) (cdr x) (list x))))
           (find-blank (string start)
             (when-let ((lpos (position #\Newline string :start start)))
               (if-let (epos (position #\Newline string :from-end T
                                       :start (1+ lpos)
                                       :end (position-if-not #'whitespacep string :start lpos)))
                 (values lpos (1+ epos))
                 (find-blank string (1+ lpos)))))
           (breakup (string start)
             (when (< start (length string))
               (multiple-value-bind (epos npos) (find-blank string start)
                 (if (null epos)
                   (list (subseq string start))
                   (cons (subseq string start epos) (breakup string npos)))))))
    (let ((strings (breakup string 0)))
      (if (cdr strings)
        `(clause ,@(mapcar (lambda (s) `(para ,@(listify (parse s 0)))) strings))
        (and strings (parse (car strings) 0))))))

(defun make-arg-reader (&rest argtypes)
  (lambda (command string start endpos)
    (let* ((pos start)
           (types argtypes)
           (args (loop while types for type = (pop types)
                   until (eql pos endpos)
                   collect (ecase type
                             (:lisp
                              (multiple-value-bind (arg nextpos) (lisp-from-string string :start pos :end endpos)
                                (setq pos nextpos)
                                arg))
                             (:identifier
                              (let* ((nextpos (or (position-if #'whitespacep string :start pos :end endpos) endpos))
                                     (arg (subseq string pos nextpos)))
                                (cassert (or (< pos nextpos) (eql pos endpos)))
                                (setq pos (or (position-if-not #'whitespacep string :start nextpos :end endpos) endpos))
                                arg))
                             ((:string :text)
                              (cassert (null types)) ;; must be last type, since means read to end.
                              (subseq string pos (setq pos endpos)))))))
      (cassert (eql pos endpos) "Too many arguments for ~s in ~s" command (string-chunk string start endpos))
      `(,command ,@args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; external ids

;; This must be called after everything defined
(defun assign-external-ids (doc)
  (let ((hash (external-ids doc))
        (dups nil))
    (cassert (eql 0 (hash-table-count hash)))
    (maphash (lambda (name clause)
               (declare (ignore name))
               (unless (eq clause doc)
                 (push clause (gethash (external-id-for-clause clause) hash))))
             (named-clauses doc))
    (maphash (lambda (simple-id clauses)
               (setf (gethash simple-id hash) (car clauses))
               (when (cdr clauses)
                 (remhash simple-id hash)
                 (setq dups (nconc (disambiguate-ids simple-id clauses) dups))))
             hash)
    ;; The disambiguated ids are unique in each group, but make sure no global conflict.
    (loop for (clause . id) in dups
      do (when (gethash id hash)
           (error "Failed to disambiguate external id for ~s and ~s" (gethash id hash) clause))
      do (setf (gethash id hash) clause))
    (maphash (lambda (id clause) (setf (clause-external-id clause) id)) hash)
    nil))

(defun disambiguate-ids (simple-id clauses)
  (cond ((every (lambda (c) (typep c 'section)) clauses)
         (labels ((assign (clauses)
                    (let ((alist ()))
                      (loop for clause in clauses
                        as label = (and clause (external-id-for-clause  clause))
                        as cell = (or (assoc label alist :test #'equal) (car (push (list label) alist)))
                        do (push clause (cdr cell)))
                      (loop for (id . subclauses) in alist
                        if (cdr subclauses)
                        nconc (disambiguate id subclauses)
                        else collect (cons (car subclauses) id))))
                  (disambiguate (id clauses)
                    (let* ((parents (mapcar #'clause-parent clauses)))
                      (when (< (length (remove-duplicates parents)) (length parents))
                        ;; There are duplicates among parents, i.e. there are clauses with the same parent
                        ;; and same simple-id. This means their titles can only differ in special characters
                        (error "Not implemented: disambiguate when titles differ only in special characters in ~s" clauses))
                      (let ((alist (assign parents)))
                        (loop for cell in alist as (parent . parent-id) = cell
                          do (setf (car cell) (find parent clauses :key #'clause-parent))
                          do (setf (cdr cell) (if parent-id  (concatenate 'string id "_in_" parent-id) id)))
                        alist))))
           (disambiguate simple-id clauses)))
        (t
         ;; For dspec's we might want to add package info... Or forget being clever and just add _1 _2 etc...
         (error "DWIM to disambiguate external ids for this case not implemented yet: ~s" clauses))))

;; An external id is a string starting with letter or underscore and containing only letters,
;; numbers, underscores, dashes and periods
;; This produces a candidate name which may be ambiguous
(defun norm-for-external-id (char string)
  ;; Remove all special characters and replace with char
  (string-downcase
   (substitute char #\space
               (normalize-whitespace
                (substitute-if-not #\space (lambda (ch) (or (alphanumericp ch) (eql ch #\-) (eql ch #\.))) string)))))

(defun external-id-for-name (name)
  (cond ((dspecp name)
         (concatenate 'string
                      (id-prefix-for-dspec-type (dspec-type name))
                      ;; TODO: what about packages? 
                      (norm-for-external-id #\_ (external-id-string (dspec-name name)))))
        ((stringp name)
         (norm-for-external-id #\_ name))
        ((null name) (error "Shouldn't need external id for toplevel document"))
        (t
         (assert (section-name-p name))
         ;; Use #\- instead of #\_, partly for backward compatibility, but also to minimize chances of title/glossary conflicts.
         (norm-for-external-id #\- (if (table-name-p name) (cadar name) (car name))))))

(defmethod external-id-for-clause (clause)
  (external-id-for-name (clause-name clause)))

(defmethod external-id-for-clause ((clause table))
  (concatenate 'string "tab_" (call-next-method)))

(defmethod external-id-for-clause ((clause document))
  nil)

(defun external-id-string (lisp-name)
  (labels ((despec (string alist)
             (loop for (spec . rep) in alist
               as pos = (search spec string) until pos
               finally (return (when pos
                                 (let ((end1 pos)
                                       (start2 (+ pos (length spec))))
                                   (when (and (> end1 0) (alphanumericp (char string (1- end1))))
                                     (setq rep (concatenate 'string "_" rep)))
                                   (when (and (< start2 (length string)) (alphanumericp (char string start2)))
                                     (setq rep (concatenate 'string rep "_")))
                                   (setq string (despec (concatenate 'string (subseq string 0 end1) rep (subseq string start2)) alist))))))
             string))
    (if (stringp lisp-name)
      ;; this is for reader macros.
      (despec lisp-name '(("#" . "sharp") ("/" . "slash") ("$" . "dollar") (">" . "gt") ("&" . "amp")))
      (let ((string (with-standard-io-syntax (princ-to-string lisp-name))))
        (despec string '(("<=" . "le") (">=" . "ge")
                         ("/=" . "ne") ("<" . "lt")
                         (">" . "gt") ("=" . "eq")
                         ("%" . "."))
                )))))
        
;; Returns a form representing the clause name NAME.
(defun text-for-clause-name (name &optional (package *section-package*))
  (cond ((stringp name) name)
        ((dspecp name) ;; including wild
         (with-standard-io-syntax
	   (let ((*package* package)
		 (*print-case* :downcase))
               (let ((lisp-name (dspec-name name)))
                 ;; TODO: do better if lisp-name is an expression (i.e. a SETF name or METHOD name)
                 ;; only happens for SETF names and METHOD names, very few times for now.
                 `(code ,(prin1-to-string lisp-name))))))
        ;; Sigh.  This happens because we compute the reference text too early...
        ((equal name '(:index)) "Index")
        ((equal name '(:glossary)) "Glossary")
        (t (let ((title (car name)))
             (if (and (consp title) (operator= (car title) :table))
               (cadr title)
               title)))))
