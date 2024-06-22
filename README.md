# CCLDoc documentation system

CCLDoc is a system for creating Lisp documentation. It uses S-expressions to represent document structure, markup, cross references, and contents. It has a small number of basic operators, supports macros for syntax extensions, and supports a simple syntax for embedding expressions in strings for added convenience.

The documentation for Clozure CL is written in CCLDoc.  The documentation files themselves are included in the CCL repository (see https://github.com/Clozure/ccl/tree/master/doc/manual).

After cloning the CCLDoc repository, load it into CCL by doing the following:
```
(require 'asdf)
(push "</path/to/ccldoc>/source/" asdf:*central-registry*)
(asdf:load-system 'ccldoc)
```

At this point, you can load the CCL manaul with
```
(defparameter *d* (ccldoc:load-document "ccl:doc;manual;ccl.ccldoc"))
```

To format the manaul, evaluate
```
(ccldoc:output-html *d* "ccl.html" :stylesheet "ccl:doc;manual;style.css")
```

You can then view the generated ccl.html file in your browser.

There is also a not-quite-complete LaTeX converter. To use it, do `(output-latex *d* "ccl.tex")` and then process the ccl.tex file with xelatex to produce a PDF file. The version of xelatex that comes with MacTeX 2014 has worked in the past.
