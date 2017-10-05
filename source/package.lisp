(defpackage ccldoc
  (:use :cl)
  (:import-from :alexandria
                #:when-let* #:when-let #:if-let
                #:starts-with-subseq)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :ccl
                #:pkg-arg #:no-such-package
                #:report-condition
                #:definition-base-name #:definition-type-instance #:method-def-parameters
                #:non-nil-symbolp #:setf-function-name-p
                #:parse-macro-1
                #:record-arglist
                #:cheap-eval-in-environment
                #:read-recording-source #:*loading-toplevel-location* #:*nx-source-note-map*
                #:block-like #:progn-print #:*print-right-margin*
                #:*show-condition-context*)
  ;;; Syntax.  Don't really need to export these, but might as well collect them in one place
  (:export
   ;; operators
   :include-file
   :defsection :index-section :glossary-section
   :block :code-block :glossentry :definition
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
   #:block
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
