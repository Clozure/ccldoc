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

(defsystem "ccldoc"
    :depends-on ("alexandria"
		 "split-sequence"
		 "s-xml"
		 "cl-who")
    :serial t
    :components ((:file "package")
		 (:file "utils")
		 (:file "representation")
		 (:file "toplevel")
		 (:file "syntax")
		 (:file "output-docbook")
		 (:file "output-tracwiki")
		 (:file "output-html")
		 (:file "output-ccldoc")))
