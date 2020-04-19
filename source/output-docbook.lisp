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


(defun ccldoc:output-docbook (doc filename &key external-format (if-exists :supersede))
  (let ((form (generate-docbook-form doc)))
    (with-open-file (s filename :direction :output :if-exists if-exists :external-format external-format)
      (write-string "<?xml version=\"1.0\" encoding=\"utf-8\"?>
                    <!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.5//EN\" \"http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd\">
                    
                    " s)
      (let ((*whitespace-allowed-tags* '(:|book| :|bookinfo| :|chapter| :|section| :|refsect1| :|refsect2| :|refsect3| :|blockquote|
                                         :|table| :|tgroup| :|tbody| :|row| :|glossentry| :|varlistentry|
                                         :|itemizedlist| :|orderedlist| :|simplelist| :|variablelist| :|refentry| :|refnamediv|)))
        (s-xml:print-xml form :stream s :pretty t))
      (truename s))))


(defmethod generate-docbook-list ((list list))
  (loop for clause in list append (generate-docbook-list clause)))

(defmethod generate-docbook-list ((clause t))
  (when-let ((form (generate-docbook-form clause)))
    (if (and (consp form) (equal (car form) '(:|phrase|)))
      (cdr form)
      (list form))))

(defun generate-body-list (clause-with-body)
  (generate-docbook-list (clause-body clause-with-body)))

(defun docbook-tag (clause tag-name)
  (if (typep clause 'named-clause)
    (list tag-name :|id| (clause-external-id clause))
    (list tag-name)))

(defun docbook-title* (clause) ;; nil or list, for use with ,@
  (and (typep clause 'clause-with-title)
       (let ((title (clause-title clause)))
         (and title
              (list `((:|title|) ,title))))))

(defmethod generate-docbook-form ((clause cons))
  `((:|phrase|) ,@(generate-docbook-list clause)))

(defmethod generate-docbook-form ((clause docerror))
  `((:|emphasis| :|role| "strong") "Error: " ,(clause-text clause)))

(defmethod generate-docbook-form ((clause document))
  `((:|book| :|lang| "en")
    ((:|bookinfo|) ,@(docbook-title* clause))
    ,@(generate-body-list clause)))

(defmethod generate-docbook-form ((clause section))
  (let* ((sect-level (section-level clause))
         (parent (ancestor-of-type clause 'definition))
         (tag (if (null parent)
                (if (eql sect-level 1)
                  :|chapter|
                  :|section|)
                (case (- sect-level (section-level parent))
                  (1 :|refsect1|)
                  (2 :|refsect2|)
                  (3 :|refsect3|)
                  (t (error "section nesting too deep for docbook, in ~s" parent))))))
    `(,(docbook-tag clause tag)
      ,@(docbook-title* clause)
      ,@(generate-body-list clause))))

(defmethod generate-docbook-form ((clause index-section))
  `(,(docbook-tag clause :|index|)
    ,@(docbook-title* clause)))
  
(defmethod generate-docbook-form ((clause glossary-section))
  `(,(docbook-tag clause :|glossary|)
    ,@(docbook-title* clause)
    ,@(generate-body-list clause)))

(defmethod generate-docbook-form ((clause para))
  `(,(docbook-tag clause :|para|)
    ,@(generate-body-list clause)))

(defmethod generate-docbook-form ((clause string))
  clause)

(defmethod generate-docbook-form ((clause text-block))
  `(,(docbook-tag clause :|blockquote|)
    ,@(docbook-title* clause)
    ,@(generate-body-list clause)))

(defmethod generate-docbook-form ((clause code-block))
  `(,(docbook-tag clause :|programlisting|)
    ,@(generate-body-list clause)))

(defun docbook-tag-for-markup (type)
  (ecase type
    (:emphasis :|emphasis|)
    (:system :|code|)
    (:sample :|replaceable|)
    (:param :|parameter|)
    (:code :|literal|)))

(defmethod generate-docbook-form ((clause markup))
  `(,(docbook-tag clause (docbook-tag-for-markup (markup-type clause)))
    ,@(generate-body-list clause)))

;;; TODO: maybe change TABLE so it stores the head row separately?
(defmethod generate-docbook-form ((clause table))
  (let* ((items (clause-items clause))
         (num-cols (reduce #'max items
                           :key #'(lambda (row) (length (clause-items row)))
                           :initial-value 0))
         (rows (map 'list #'generate-docbook-form items)))
    `(,(docbook-tag clause :|table|)
      ,@(docbook-title* clause)
      ((:|tgroup| :|cols| ,(princ-to-string num-cols))
       ((:|thead|) ,(car rows))
       ((:|tbody|) ,@(cdr rows))))))

(defmethod generate-docbook-form ((clause row))
  `(,(docbook-tag clause :|row|)  
    ,@(map 'list (lambda (item)
                   `((:|entry|) ,@(generate-docbook-list item)))
           (clause-items clause))))

(defmethod generate-docbook-form ((clause glossentry))
  `(,(docbook-tag clause :|glossentry|)
    ((:|glossterm|) ,@(generate-docbook-list (clause-term clause)))
    ((:|glossdef|) ,@(generate-body-list clause))))

(defmethod generate-docbook-form ((clause link))
  `((:|ulink| :|url| ,(link-url clause)) ,@(generate-body-list clause)))

(defun docbook-tags-for-listing (type)
  (ecase type
    (:bullet (values '(:|itemizedlist|) '(:|listitem|)))
    (:number (values '(:|orderedlist|) '(:|listitem|)))
    (:column (values '(:|simplelist|) '(:|member|)))
    (:definition (values '(:|variablelist|) '(:|varlistentry|)))))

(defmethod generate-docbook-form ((clause listing))
  (multiple-value-bind (tag subtag) (docbook-tags-for-listing (listing-type clause))
    `((:|blockquote|)
      (,tag
       ,@(map 'list (lambda (item)
                      `(,subtag ,@(generate-docbook-list item)))
              (clause-items clause))))))
      

(defmethod generate-docbook-form ((clause term-item))
  `((:|phrase|)
    ((:|term|) ,@(generate-docbook-list (clause-term clause)))
    ((:|listitem|) ,@(generate-body-list clause))))

(defmethod generate-docbook-form ((clause item))
  (when-let (body (clause-body clause))
    (generate-docbook-form body)))

(defmethod generate-docbook-form ((clause definition))
  (let* ((name-clause (definition-display-name clause))
         (name-text (clause-text name-clause))
         (name-form (generate-docbook-list name-clause)))
    `(,(docbook-tag clause :|refentry|)
      ((:|indexterm| :|zone| ,(clause-external-id clause)) (:|primary| ,@name-form))
      ((:|refnamediv|)
       ((:|refname|) ,name-text)
       ((:|refpurpose|) ,@(generate-docbook-list (definition-summary clause)))
       ((:|refclass|) ,(dspec-type-name (clause-name clause))))
      ((:|refsynopsisdiv|) ((:|synopsis|) ,@(generate-docbook-list (definition-signature clause))))
      ,@(generate-body-list clause))))

(defmethod generate-docbook-form ((clause xref))
  (let* ((target (xref-target clause))
         (term (generate-body-list clause))
         (defaulted-term (or term (generate-docbook-list (xref-default-body clause))))
         (id (clause-external-id target)))
    (etypecase target
      (glossentry
       `((:|phrase|)
         ((:|indexterm|) ((:|primary|) ,@defaulted-term))
         ((:|glossterm| :|linkend| ,id) ,@defaulted-term)))
      (definition
          `((:|phrase|)
            ((:|indexterm|) ((:|primary|) ,@defaulted-term))
            ((:|link| :|linkend| ,id) ,@defaulted-term)))
      ((or section table)
       (if term
         `((:|link| :|linkend| ,id) ,@term)
         `((:|xref| :|linkend| ,id)))))))

(defmethod generate-docbook-form ((clause indexed-clause))
  (let ((term (generate-body-list clause)))
    `((:|phrase|)
      ((:|indexterm|) ((:|primary|) ,@term))
      ,@term)))
