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

\\usepackage{listings}
\\lstset{basicstyle=\\small\\ttfamily,
  showspaces=false,
  showstringspaces=false,
  columns=flexible,
  language={}}

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
  
(defmethod write-latex ((clause index-section) stream))
(defmethod write-latex ((clause glossary-section) stream))
(defmethod write-latex ((clause glossentry) stream))

(defmethod write-latex ((clause section) stream)
  (cond ((typep (ancestor-of-type clause 'named-clause) 'definition)
	 ;; don't write section titles in defintions
	 (write-latex (clause-body clause) stream))
	((<= (section-level clause) 1)
	 ;; heuristic: a top-level section clause is a chapter
	 (format stream "~&\\chapter{")
	 (write-latex (clause-title clause) stream)
	 (format stream "}~2%"))
	((= (section-level clause) 2)
	 (format stream "~&\\section{")
	 (write-latex (clause-title clause) stream)
	 (format stream "}~2%")))
  (write-latex (clause-body clause) stream))

(defmethod write-latex ((clause code-block) stream))
(defmethod write-latex ((clause block) stream))

(defmethod write-latex ((clause para) stream)
  ;; paragraphs in TeX are delimited by blank lines
  (format stream "~&~%")
  (write-latex (clause-body clause) stream)
  (format stream "~&~%"))

(defmethod write-latex ((clause docerror) stream))
(defmethod write-latex ((clause link) stream))
(defmethod write-latex ((clause table) stream))
(defmethod write-latex ((clause row) stream))
(defmethod write-latex ((clause listing) stream))
(defmethod write-latex ((clause indexed-clause) stream))

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

(defmethod write-latex ((clause item) stream))
(defmethod write-latex ((clause term-item) stream))
(defmethod write-latex ((clause xref) stream))
(defmethod write-latex ((clause definition) stream))

(defmethod write-latex ((clause cons) stream)
  (dolist (c clause)
    (write-latex c stream)))

(defmethod write-latex ((clause string) stream)
  (write-string (escape-for-latex clause) stream))

(defmethod write-latex ((clause null) stream)
  (declare (ignore stream))
  (warn "null clause"))
