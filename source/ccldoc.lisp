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


;; Master ccldoc file.  Load this file to load ccldoc.


;; Override whatever old asdf swank may have dragged in, and never again load anything
;; from any "system" common lisp directories
(load "ccl:tools;asdf")
(setq asdf:*default-source-registries*
      (remove-if (lambda (s) (and (symbolp s) (search "SYSTEM-SOURCE-REGISTRY" (string s))))
                 asdf:*default-source-registries*))
(asdf:initialize-source-registry)

(unless (member :quicklisp *features*) (load "~/quicklisp/setup.lisp"))

;; Don't trust foreign libraries to not mung global optimization settings.
(ccl:advise ql:quickload
            (let ((ccl::*nx-speed* ccl::*nx-speed*)
                  (ccl::*nx-space* ccl::*nx-space*)
                  (ccl::*nx-safety* ccl::*nx-safety*)
                  (ccl::*nx-debug* ccl::*nx-debug*))
              (:do-it))
            :name :preserve-compiler-optimization-settings
            :when :around)

(ccl:advise asdf::module-provide-asdf
            (let ((ccl::*nx-speed* ccl::*nx-speed*)
                  (ccl::*nx-space* ccl::*nx-space*)
                  (ccl::*nx-safety* ccl::*nx-safety*)
                  (ccl::*nx-debug* ccl::*nx-debug*))
              (:do-it))
            :name :preserve-compiler-optimization-settings
            :when :around)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup

(ql:quickload "ccldoc-libraries")

(defpackage ccldoc
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

(defparameter *ccldoc-files* '("utils"
                               "representation"
                               "toplevel"
                               "syntax"
                               "output-docbook"
                               "output-tracwiki"
                               "output-ccldoc"))


(loop for file in *ccldoc-files*
  do (load (merge-pathnames file *loading-file-source-file*)))

(provide 'ccldoc)








