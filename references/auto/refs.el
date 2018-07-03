(TeX-add-style-hook
 "refs"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1in")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "hyperref"
    "amsmath"
    "amsfonts"
    "scalerel"
    "graphicx"
    "enumerate"
    "xcolor"
    "caption"
    "subcaption"
    "listings")
   (TeX-add-symbols
    '("distras" 1)
    '("distas" 1)
    "rz")
   (LaTeX-add-labels
    "fig3c"
    "hists"
    "tabs1"
    "residsf"
    "diagsf")
   (LaTeX-add-bibliographies
    "Master")
   (LaTeX-add-saveboxes
    "mybox"
    "mysim"))
 :latex)

