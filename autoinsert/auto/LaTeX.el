(TeX-add-style-hook "LaTeX"
 (lambda ()
    (TeX-run-style-hooks
     "sectsty"
     "enumerate"
     "fancyhdr"
     "tikz"
     "graphicx"
     "courier"
     "listings"
     "cancel"
     "amsmath"
     "geometry"
     "margin=2.0cm"
     "latex2e"
     "art10"
     "article"
     "")))

