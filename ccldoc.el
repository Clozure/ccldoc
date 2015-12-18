;;; -*- emacs-lisp -*-

;;; This file contains code to set up Emacs to edit the Clozure Common Lisp
;;; Documentation (CCLDoc) files.

(define-minor-mode ccldoc
  "The Clozure Common Lisp Documentation (CCLDoc) minor mode."
  nil " CCLDoc" nil
  (put 'chapter 'lisp-indent-function 1)
  (put 'defsection 'lisp-indent-function 1)
  (put 'definition 'lisp-indent-function 3)
  (put 'table 'lisp-indent-function 1))

(add-hook 'lisp-mode-hook (lambda ()
                            (when (string-match-p "[[:alnum:][:space:]]\\.ccldoc$"
                                                  (buffer-name))
                              (ccldoc))))
