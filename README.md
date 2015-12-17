# CCLDoc documentation system

CCLDoc is a system for creating Lisp documentation. It uses S-expressions to represent document structure, markup, cross references, and contents. It has a small number of basic operators, supports macros for syntax extensions, and supports a simple syntax for embedding expressions in strings for added convenience.

The documentation for Clozure CL is written in CCLDoc.

To use CCLDoc to format the CCL documentation, follow these steps:

1. Install CCL.  See http://ccl.clozure.com/download.html
2. Install Quicklisp.  See http://www.quicklisp.org
3. Check out the CCLDoc sources into ~/quicklisp/local-projects

Now, start CCL, and do the following:

    (load "home:quicklisp;setup")
    (ql:quickload :ccldoc)
    (defparameter *d* (ccldoc:load-document "ccl:doc;manual;ccl.ccldoc"))
    (ccldoc::output-html *d* "/tmp/ccl.html" :stylesheet "ccl.css")

You can then view the generated ccl.html file in your browser. The generated HTML expects to use a style file named ccl.css in the same directory.

There is also a not-quite-complete LaTeX converter. To use it, do `(output-latex *d* "ccl.tex")` and then process the ccl.tex file with xelatex to produce a PDF file. The version of xelatex that comes with MacTeX 2014 works.
