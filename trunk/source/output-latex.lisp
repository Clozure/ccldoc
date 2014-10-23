(in-package :ccldoc)

(defun output-latex (doc filename &key external-format (if-exists :supersede))
  (with-open-file (s filename :direction :output :if-exists if-exists
		     :external-format external-format)
    (write-latex doc s)
    (truename s)))

;;; There are 10 special characters: \ { } _ ^ # & $ % ~
;;; \ => \textbackslash{}
;;; ^ => \textasciicircum{} or \^{}
;;; ~ => \textasciitilde{} or \~{}
;;; It suffices to escape the remainder with \
(defun escape-latex-string (string))

(defmethod write-latex ((clause document) stream))
(defmethod write-latex ((clause index-section) stream))
(defmethod write-latex ((clause glossary-section) stream))
(defmethod write-latex ((clause glossentry) stream))
(defmethod write-latex ((clause section) stream))
(defmethod write-latex ((clause code-block) stream))
(defmethod write-latex ((clause block) stream))
(defmethod write-latex ((clause para) stream))
(defmethod write-latex ((clause docerror) stream))
(defmethod write-latex ((clause link) stream))
(defmethod write-latex ((clause table) stream))
(defmethod write-latex ((clause row) stream))
(defmethod write-latex ((clause listing) stream))
(defmethod write-latex ((clause indexed-clause) stream))
(defmethod write-latex ((clause markup) stream))
(defmethod write-latex ((clause item) stream))
(defmethod write-latex ((clause term-item) stream))
(defmethod write-latex ((clause xref) stream))
(defmethod write-latex ((clause definition) stream))
(defmethod write-latex ((clause cons) stream))
(defmethod write-latex ((clause string) stream))


