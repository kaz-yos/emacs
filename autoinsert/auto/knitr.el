(TeX-add-style-hook "knitr"
 (lambda ()
    (TeX-run-style-hooks
     "enumerate"
     "sectsty"
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

