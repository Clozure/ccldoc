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

(defsystem "ccldoc"
  :author "Clozure Associates and contributors"
  :description "create lisp documentation using s-expressions"
  :license "Apache License 2.0"
  :depends-on ("alexandria"
               "split-sequence"
               "s-xml"
               "cl-who"
               #-ccl "ccl-compat")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "representation")
               (:file "toplevel")
               (:file "syntax")
               (:file "output/docbook")
               (:file "output/tracwiki")
               (:file "output/html")
               (:file "output/latex")
               (:file "output/ccldoc")))
