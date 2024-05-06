(in-package :ccldoc)

;;; if-let, when-let, remove-from-plist and starts-with-subseq are
;;; copied from alexadria, which is in the public domain

(defmacro if-let (bindings &body (then-form &optional else-form))
    "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defun remove-from-plist (plist &rest keys)
  "Returns a property-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  ;; FIXME: possible optimization: (remove-from-plist '(:x 0 :a 1 :b 2) :a)
  ;; could return the tail without consing up a new list.
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
        collect key and collect (first rest)))

(defun starts-with-subseq (prefix sequence &rest args
                           &key
                           (return-suffix nil return-suffix-supplied-p)
                           &allow-other-keys)
  "Test whether the first elements of SEQUENCE are the same (as per TEST) as the elements of PREFIX.

If RETURN-SUFFIX is T the function returns, as a second value, a
sub-sequence or displaced array pointing to the sequence after PREFIX."
  (declare (dynamic-extent args))
  (let ((sequence-length (length sequence))
        (prefix-length (length prefix)))
    (when (< sequence-length prefix-length)
      (return-from starts-with-subseq (values nil nil)))
    (flet ((make-suffix (start)
             (when return-suffix
               (cond
                 ((not (arrayp sequence))
                  (if start
                      (subseq sequence start)
                      (subseq sequence 0 0)))
                 ((not start)
                  (make-array 0
                              :element-type (array-element-type sequence)
                              :adjustable nil))
                 (t
                  (make-array (- sequence-length start)
                              :element-type (array-element-type sequence)
                              :displaced-to sequence
                              :displaced-index-offset start
                              :adjustable nil))))))
      (let ((mismatch (apply #'mismatch prefix sequence
                             (if return-suffix-supplied-p
                                 (remove-from-plist args :return-suffix)
                                 args))))
        (cond
          ((not mismatch)
           (values t (make-suffix nil)))
          ((= mismatch prefix-length)
           (values t (make-suffix mismatch)))
          (t
           (values nil nil)))))))


;;; adapted from cl-who

;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defun html-escape-char-test (ch)
  (or (find ch "<>&'\"")
      (> (char-code ch) 127)))

(defun html-escape-char-minimal-test (ch)
  (find ch "<>&"))

(defun escape-for-html (string &key (test 'html-escape-char-test))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string)))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string) 
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos 
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")                 
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s "&#~d;" (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defun escape-for-html-minimal (string)
  (escape-for-html string :test 'html-escape-char-minimal-test))

;;; from example in https://lispcookbook.github.io/cl-cookbook/strings.html
(defun simple-split (delimiter string)
  (loop for start = 0 then (1+ delimiter-pos)
        as delimiter-pos = (position delimiter string :start start)
        collect (subseq string start delimiter-pos)
        while delimiter-pos))

