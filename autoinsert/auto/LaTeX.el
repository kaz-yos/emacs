(TeX-add-style-hook
 "LaTeX"
 (lambda ()
    (TeX-run-style-hooks
     "latex2e"
     ""
     "article"
     "art10"
     "margin=2.0cm"
     "geometry"
     "amsmath"
     "cancel"
     "listings"
     "courier"
     "graphicx"
     "tikz"
     "fancyhdr"
     "sectsty"
     "enumerate")))

