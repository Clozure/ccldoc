(in-package :ccldoc)

(defun output-latex (doc filename &key external-format (if-exists :supersede))
  (with-open-file (s filename :direction :output :if-exists if-exists
		     :external-format external-format)
    (write-latex doc s)
    (truename s)))

(defun write-latex-preamble (stream)
  (write-string "
\\documentclass[twoside,headings]{book}
\\usepackage[papersize={6.25in,9.25in}]{geometry} % CLTL2 is this size
\\usepackage{mathspec,xltxtra,xunicode}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont[Mapping=tex-text]{Times-Roman}
\\setsansfont[Scale=MatchLowercase,Mapping=tex-text]{Eras Demi ITC}
\\setmonofont[Scale=MatchLowercase]{Inconsolata}
\\setmathfont(Digits){Times-Roman}
\\usepackage{sectsty}
\\chaptertitlefont{\\sf}
\\chapterfont{\\sf}
\\usepackage{longtable}
\\usepackage{listings}
\\lstset{basicstyle=\\small\\ttfamily,
  showspaces=false,
  showstringspaces=false,
  columns=flexible,
  frame=single,
  language={}}

\\def\\cf{\\tt\\frenchspacing}

\\input{defun}

% make this the last \usepackage
\\usepackage[pdfborder={0 0 0}]{hyperref}

" stream))



;;; There are 10 special characters: \ { } _ ^ # & $ % ~
;;; \ => \textbackslash{}
;;; ^ => \textasciicircum{} or \^{}
;;; ~ => \textasciitilde{} or \~{}
;;; It suffices to escape the remainder with \

(defun escape-char (char)
  (case char
    (#\\ "\\textbackslash{}")
    (#\{ "\\{")
    (#\} "\\}")
    (#\_ "\\_")
    (#\^ "\\textasciicircum{}")
    (#\# "\\#")
    (#\& "\\&")
    (#\$ "\\$")
    (#\% "\\%")
    (#\~ "\\textasciitilde{}")
    (otherwise char)))

(defun escape-char-test (char)
  (find char "\\{}_^#&$%~" :test 'char=))

;; adapted from cl-who
(defun escape-for-latex (string &key (test 'escape-char-test))
  (let ((first-pos (position-if test string)))
    (if (not first-pos)
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
	         (write-sequence (escape-char char) s)
	      while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defmethod write-latex ((clause document) stream)
  (write-latex-preamble stream)
  (format stream "\\begin{document}~%")
  (format stream "\\tableofcontents~%")
  (write-latex (clause-body clause) stream)
  (format stream "~&\\end{document}~%"))

(defmethod write-latex :before ((clause named-clause) stream)
  (when (and (clause-name clause)
	     (not (typep (ancestor-of-type clause 'named-clause) 'definition)))
    (format stream "\\label{~a}~%" (clause-external-id clause))))
	   
(defmethod write-latex ((clause index-section) stream))

(defmethod write-latex ((clause indexed-clause) stream)
  (write-latex (clause-body clause) stream)
  (format stream "\\index{~a} " (clause-body clause)))

(defmethod write-latex ((clause glossary-section) stream))

(defmethod write-latex ((clause glossentry) stream)
  (write-latex (clause-body clause) stream)
  (format stream "\\newglossaryentry{~a}" (clause-term clause))
  (format stream "{name={~a},description={~a}}" (clause-term clause)
	  (clause-body clause)))

(defmethod write-latex ((clause section) stream)
  (cond ((typep (ancestor-of-type clause 'named-clause) 'definition)
	 ;; don't write section titles in definitions
	 nil)
	((<= (section-level clause) 1)
	 ;; heuristic: a top-level section clause is a chapter
	 (format stream "~&\\chapter{")
	 (write-latex (clause-title clause) stream)
	 (format stream "}~2%"))
	((= (section-level clause) 2)
	 (format stream "~&\\section{")
	 (write-latex (clause-title clause) stream)
	 (format stream "}~2%"))
	((= (section-level clause) 3)
	 (format stream "~&\\subsection{")
	 (write-latex (clause-title clause) stream)
	 (format stream "}~2%"))
	((= (section-level clause) 4)
	 (format stream "~&\\subsubsection{")
	 (write-latex (clause-title clause) stream)
	 (format stream "}~2%")))
  (write-latex (clause-body clause) stream))

(defmethod write-latex ((clause code-block) stream)
  (format stream "~&\\begin{lstlisting}~%")
  ;; Gobble leading and trailing newlines, which take up extra space
  ;; in the TeX output.
  (let ((s (make-string-output-stream)))
    (write-latex (clause-body clause) s)
    (write-string (string-trim '(#\newline) (get-output-stream-string s))
		  stream))
  (format stream "~&\\end{lstlisting}~%"))

(defmethod write-latex ((clause block) stream)
  (format stream "~&\\begin{quotation}~%")
  (write-latex (clause-body clause) stream)
  (format stream "~&\\end{quotation}~2%"))

(defmethod write-latex ((clause para) stream)
  ;; paragraphs in TeX are delimited by blank lines
  (format stream "~&~%")
  (write-latex (clause-body clause) stream)
  (format stream "~&~%"))

(defmethod write-latex ((clause docerror) stream))

(defmethod write-latex ((clause link) stream)
  (format stream "\\href{~a}" (link-url clause))
  (write-string "{" stream)
  (write-latex (clause-body clause) stream)
  (write-string "} " stream))

(defmethod write-latex ((clause table) stream)
  (format stream "~&\\begin{center}")
  (format stream "~&\\begin{longtable}")
  (let* ((rows (clause-items clause))
	 (ncol (length (clause-items (elt rows 0)))))
    (write-string "{" stream)
    (dotimes (i ncol)
      (write-char #\l stream))
    (format stream "}~%")
    (dotimes (i (length rows))
      (write-latex (aref rows i) stream))
    (format stream "~&\\caption{")
    (write-latex (clause-title clause) stream)
    (write-string "}" stream)
    (format stream "~&\\end{longtable}")
    (format stream "~&\\end{center}~%")))

(defmethod write-latex ((clause row) stream)
  (let* ((items (clause-items clause))
	 (nitems (length items))
	 (last-index (1- nitems)))
    (dotimes (i nitems)
      (write-latex (aref items i) stream)
      (if (< i last-index)
	(write-string " & " stream)
	(format stream " \\\\~%")))))


(defmethod write-latex ((clause listing) stream)
  (let ((environment (case (listing-type clause)
		       (:bullet "itemize")
		       (:number "enumerate")
		       (:definition "description"))))
    (when environment
      (format stream "~&\\begin{~a}~%" environment))
    (loop for item across (clause-items clause)
	  do (progn
	       (when (member (listing-type clause) '(:bullet :number))
		 (write-string "\\item " stream))
	       (write-latex item stream)))
    (when environment
      (format stream "~&\\end{~a}~2%" environment))))
  

(defmethod write-latex ((clause markup) stream)
  (let ((code (ecase (markup-type clause)
		(:emphasis "\\emph")
		(:code "\\texttt")
		(:param "\\textit")
		(:sample "\\textit")
		(:system "\\texttt"))))
    (format stream "~a{" code)
    (write-latex (clause-body clause) stream)
    (write-string "}" stream)))

(defmethod write-latex ((clause item) stream)
  (let ((body (clause-body clause)))
    (when body
      (write-latex body stream)
      (fresh-line stream))))

(defmethod write-latex ((clause term-item) stream)
  (format stream "\\item[~a] " (clause-term clause))
  (write-latex (clause-body clause) stream)
  (fresh-line stream))

(defmethod write-latex ((clause xref) stream)
  (format stream "\\hyperref[~a]" (clause-external-id (xref-target clause)))
  (write-string "{" stream)
  (write-latex (or (clause-body clause)
		   (xref-default-body clause)) stream)
  (write-string "}" stream))

(defmethod write-latex ((clause definition) stream)
  (let ((kind (dspec-type-name (clause-name clause))))
    ;; kludge: wrap hemlock variable signatures in hbox like we
    ;; do for macros
    (when (or (string= kind "Hemlock Variable")
	      (string= kind "Hemlock Command")
	      (string= kind "Toplevel Command")
	      (string= kind "Lap Macro"))
      (setq kind "Macro"))
    (format stream "~&\\begin{defun}")
    (format stream "[~a]~%" kind)
    (flet ((escape-signature-test (char)
	     (or (char= char #\#)
		 (char= char #\%)
		 (char= char #\$)
		 (char= char #\_))))
      (when (string= kind "Macro")
	(write-string "\\hbox{" stream))
      (write-string (escape-for-latex
		     (string-trim '(#\newline #\space)
				  (clause-text (definition-signature clause)))
		     :test #'escape-signature-test)
		    stream)
      (when (string= kind "Macro")
	(write-string "}" stream)))
    (format stream "~2%")
    (when (definition-summary clause)
      (write-latex (definition-summary clause) stream)
      (format stream "~&~%"))
    (let ((s (make-string-output-stream)))
      (write-latex (clause-body clause) s)
      (write-string (string-trim '(#\newline) (get-output-stream-string s))
		    stream))
    (format stream "~&\\end{defun}~2%")))

(defmethod write-latex ((clause cons) stream)
  (dolist (c clause)
    (write-latex c stream)))

(defmethod write-latex ((clause string) stream)
  (write-string (escape-for-latex clause) stream))

(defmethod write-latex ((clause null) stream)
  (declare (ignore stream))
  (warn "null clause"))
