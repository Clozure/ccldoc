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

(defmacro cassert (form &rest error-args)
  `(unless ,form
     (restart-case (error ,@(or error-args `("Assertion failed ~s" ',form)))
       (continue ()
         :report "Ignore the assertion"
         nil))))

(defmacro def-predicate (name type)
  (let ((obj (gensym)))
    `(defun ,name (,obj) (typep ,obj ',type))))


;; Treat uninterned symbols as string, so as to provide an alternative syntax for entering text with
;; many double quotes.
(defun gensymp (thing)
  (and (symbolp thing) (null (symbol-package thing))))

(defun desym (thing)
  (if (gensymp thing)
    (symbol-name thing)
    thing))

;; this went through many iterations, but currently settled on keywords...
(defun op-name (sym &optional intern)
  (and (symbolp (desym sym))
       (if intern
         (intern (symbol-name sym) :keyword)
         (find-symbol (symbol-name sym) :keyword))))

(defun op-name-p (sym1 sym2)
  (cassert (keywordp sym2))
  (and (symbolp (desym sym1))
       (string= (symbol-name sym1) (symbol-name sym2))))

(defun normalize-whitespace (string)
  (cassert (stringp string))
  (let ((res (make-array (length string) :element-type 'character :fill-pointer 0)))
    (loop for lastch = #\x then ch for ch across string
      do (unless (whitespacep ch)
           (when (whitespacep lastch)
             (vector-push #\space res))
           (vector-push ch res)))
    (let ((reslen (fill-pointer res)))
      (subseq res (if (and (> reslen 0) (eql #\space (aref res 0))) 1 0) reslen))))

(defun concat-by (sep &rest strings)
  (let* ((sep-seq (if (typep sep 'sequence) sep (string sep)))
         (args (loop for string in strings
                 unless (eql (length string) 0) collect string and collect sep-seq)))
    (apply #'concatenate 'string (butlast args))))

(defun read-all-from-string (string &key start end package)
  (multiple-value-bind (value pos)
                       (let ((*read-eval* nil)
                             (*package* (or package *package*)))
                         ;; Read-from-string doesn't accept NIL start/end args, natch.
                         (read-from-string string t nil :start (or start 0) :end (or end (length string))))
    (cassert (eql pos (or end (length string))))
    value))

;; Need to be able to support lisp names that include symbols in packages that don't exist in the current image.
;; For now, this horrible kludge...  
;; TODO: make sure symbols not needed once DOM is built, and delete the fake packages once compilation done.
(defvar *ccldoc-fake-packages* nil)

(defun ccldoc-package (name)
  (let ((package (make-package name :use nil)))
    (push package *ccldoc-fake-packages*)
    (import nil package)
    package))

(defmacro with-ccldoc-packages (&body body)
  `(loop
     (handler-case (return (progn ,@body))
       (no-such-package (c)
                        (let ((pkg-name (package-error-package c))) ; 
                          (unless (and (stringp pkg-name) (not (find-package pkg-name)))
                            (error c))
                          (ccldoc-package pkg-name)))
       (simple-error (c)
                     (let ((args (simple-condition-format-arguments c)))
                       (unless (and (search "No external symbol named ~S in package ~S" (simple-condition-format-control c))
                                    (member (cadr args) *ccldoc-fake-packages*)
                                    (stringp (car args)))
                         (error c))
                       (export (intern (car args) (cadr args)) (cadr args)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dspecs
;;;

;; a dspec is the canonicalized name of a definition.  They are used as clause name, and hence must
;; obey EQUALP as the equivalence relationship.

(defparameter *dspec-types* nil)

(defstruct (dspecinfo (:type list))
  type ;; must be first so can use assq
  type-name
  id-prefix
  parent-type
  function)

(defun register-dspec-type (type parent-type type-name id-prefix function)
  (cassert (keywordp type))
  (cassert (or (null parent-type) (keywordp parent-type)))
  (when id-prefix
    (cassert (and (stringp id-prefix)
                  (> (length id-prefix) 0)
                  (alpha-char-p (char id-prefix 0))
                  (every (lambda (c) (or (alphanumericp c) (find c "_-."))) id-prefix))
             "Invalid id-prefix: ~s" id-prefix))
  (when function
    (cassert (or (symbolp function) (typep function 'function))))
  (let* ((info (make-dspecinfo :type type
                               :type-name type-name
                               :id-prefix id-prefix
                               :parent-type parent-type
                               :function function))
         (old (assq type *dspec-types*)))
    (when (and old (not (equal old info)))
      (warn "Redefining dspec-type ~s" type))
    (when old (setq *dspec-types* (remove old *dspec-types*)))
    (push info *dspec-types*)
    type))
  
(defun dspec-type-for-type-name (type-name)
  (dspecinfo-type (find type-name *dspec-types* :key #'dspecinfo-type-name :test #'equalp)))

(defun info-for-dspec-type (type)
  (or (assq type *dspec-types*) (error "Unknown dspec type ~s" type)))

(defun dspec-type-name (dspec)
  (dspecinfo-type-name (info-for-dspec-type (dspec-type dspec))))

(defun id-prefix-for-dspec-type (type)
  (let ((info (info-for-dspec-type type)))
    (or (dspecinfo-id-prefix info)
        (when-let (parent-type (dspecinfo-parent-type info))
          (id-prefix-for-dspec-type parent-type))
        "x_")))

(defun parent-type-for-dspec-type (type)
  (dspecinfo-parent-type (info-for-dspec-type type)))

(defun function-for-dspec-type (type)
  (let ((info (info-for-dspec-type type)))
    (or (dspecinfo-function info)
        (when-let (parent-type (dspecinfo-parent-type info))
          (function-for-dspec-type parent-type))
        #'identity)))

(defun canonicalize-definition-name (type name)
  (funcall (function-for-dspec-type type) name))

(defun dspec-type-name-p (type)
  (or (eq type t) (assq type *dspec-types*)))

(defmacro ccldoc:def-definition-type (type (&optional parent-type) &key type-name id-prefix function)
  (let* ((type (and type (intern (symbol-name type) :keyword)))
         (parent-type (and parent-type (intern (symbol-name parent-type) :keyword)))
         (type-name (or type-name (let ((*print-case* :capitalize))
                                    (substitute #\Space #\- (princ-to-string type))))))
    `(register-dspec-type ,type ,parent-type ,type-name ,id-prefix ,function)))

(defun std-dspec-name (ccl-type name)
  (definition-base-name (definition-type-instance ccl-type) name))

(defun symbol-dspec-name (name)
  (and (symbolp name) name))

(defun string-dspec-name (name)
  (and (stringp name) name))

(def-definition-type :type ()
  :id-prefix "t_"
  :function #'identity)

(def-definition-type :class (:type)
  :id-prefix "c_"
  :function #'symbol-dspec-name)

(def-definition-type :condition (:class)
  :id-prefix "c_")

(def-definition-type :function () :id-prefix "f_"
  :function (lambda (name)
              (let ((name (std-dspec-name 'function name)))
                (when (or (non-nil-symbolp name) (setf-function-name-p name))
                  name))))

(def-definition-type :macro (:function)
  :id-prefix "m_")

(def-definition-type :generic-function (:function)
  :id-prefix "f_")

(def-definition-type :lap-macro (:function)
  :id-prefix "f_")

(def-definition-type :method ()
  :id-prefix "m_"
  :function (lambda (name)
              (multiple-value-bind (gf-name quals specs) (method-def-parameters name)
                (if gf-name
                  `(,gf-name ,@quals
                             ,(mapcar (lambda (s) (if (typep s 'class) (class-name s) s)) specs))
                  ;; TODO: For now allow symbols because the only use we have is with symbols, but in the future disallow this.
                  (and (symbolp name) name)))))

(def-definition-type :variable ()
  :id-prefix "v_"
  :function #'symbol-dspec-name)

(def-definition-type :reader-macro ()
  :id-prefix "r_"
  :function (lambda (name)
              (if (characterp name)
                (string name)
                (and (typep name 'sequence)
                     (every #'characterp name)
                     (<= 1 (length name) 2)
                     (coerce name 'string)))))


(def-definition-type :package ()
  :id-prefix "p_"
  :function (lambda (name) (std-dspec-name 'package name)))

(def-definition-type :toplevel-command ()
  :id-prefix "tc_"
  :function #'string-dspec-name)

(def-definition-type :hemlock-variable ()
  :id-prefix "hv_"
  :function #'string-dspec-name)

(def-definition-type :hemlock-command ()
  :id-prefix "hc_"
  :function #'string-dspec-name)


(defstruct (dspec (:constructor %make-dspec) (:predicate dspecp))
  (type t :type (satisfies dspec-type-name-p))
  name)

;; This is called with type and name as specified by the user, either in the docentry or in a reference to one.
(defun make-dspec (type name)
  (cassert (symbolp (desym type)))
  (let* ((ctype (intern (string type) :keyword))
         (cname (canonicalize-definition-name ctype name)))
    (unless cname
      (let* ((dwimmed (and (stringp name) (with-ccldoc-packages (read-all-from-string name))))
             (dwimmed-cname (and dwimmed (canonicalize-definition-name ctype dwimmed))))
        (unless dwimmed-cname
          (error "Invalid ~s definition name ~s" type name))
        (setq cname dwimmed-cname)))
    (%make-dspec :type ctype :name cname)))

(defun make-wild-dspec (name)
  ;; can't canonicalize if don't know type.
  (%make-dspec :type t :name name))

(defun wild-dspec-p (name)
  (and (dspecp name) (eq (dspec-type name) t)))

(defun dspec-subtypep (type super)
  #+gz (cassert (assq type *dspec-types*))
  #+gz (cassert (or (eq super t) (assq super *dspec-types*)))
  (or (eq super t)
      (eq type super)
      (when-let (parent (parent-type-for-dspec-type type))
        (dspec-subtypep parent super))))

