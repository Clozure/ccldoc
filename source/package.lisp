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

(defpackage ccldoc
  (:use :cl)
  (:import-from #+:ccl :ccl
                #-:ccl :ccl-compat
                #:pkg-arg #:no-such-package
                #:report-condition
                #:definition-base-name #:definition-type-instance #:method-def-parameters
                #:non-nil-symbolp #:setf-function-name-p
                #:parse-macro-1
                #:record-arglist
                #:cheap-eval-in-environment
                #:read-recording-source #:*loading-toplevel-location* #:*nx-source-note-map*
                #:block-like #:progn-print #:*print-right-margin*
                #:*show-condition-context*
                #:assq #:whitespacep #:require-type #:neq #:memq
                #:*loading-file-source-file* #:nfunction
                #:*save-source-locations* #:record-source-file)
  ;;; Syntax.  Don't really need to export these, but might as well collect them in one place
  (:export
   ;; operators
   :include-file
   :defsection :index-section :glossary-section
   :text-block :code-block :glossentry :definition
   :table :row :item :para
   :clause :index :link :markup :ref :quote
   ;; macros
   :document :chapter :section
   :variable :function :macro :type :refdef :term
   :emphasis :system :sample :param :code
   :lbrace :rbrace)
  ;;; API
  (:export
   #:load-document
   #:output-docbook
   #:output-tracwiki
   #:output-ccldoc
   #:output-html
   #:def-expander
   #:def-definition-type)
  ;;; DOM
  (:export
   ;; concrete classes
   #:document
   #:index-section
   #:glossary-section
   #:section
   #:xref #:xref-target #:xref-default-body
   #:indexed-clause
   #:link #:link-url
   #:glossentry
   #:definition #:definition-display-name #:definition-summary #:definition-signature
   #:table
   #:row
   #:listing #:listing-type
   #:code-block
   #:markup #:markup-type
   #:text-block
   #:para
   #:term-item
   #:item
   #:docerror
   ;; abstract classes
   #:clause-with-items #:clause-items
   #:clause-with-body #:clause-body
   #:clause-with-title #:clause-title
   #:clause-with-term #:clause-term
   #:named-clause #:clause-name #:clause-external-id
   #:clause-object
     #:clause-parent
     #:clause-document
     #:section-level
     #:clause-text
   ;; type
   #:clause
   ;; dspec (definition clause-name)
   #:dspec
   #:dspec-type
   #:dspec-name
   #:dspec-type-name))
