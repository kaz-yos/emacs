;;;
;;; Code folding related



;;; 
;;; fold-dwim.el
;;
;; DWIM stands for "do what I mean", as in the idea that one keystroke
;; can do different things depending on the context. In this package,
;; it means that, if the cursor is in a currently hidden folded
;; construction, we want to show it; if it's not, we want to hide
;; whatever fold the cursor is in.
(use-package fold-dwim
  :commands (fold-dwim-toggle
             fold-dwim-hide-all
             fold-dwim-show-all)
  ;; :bind (("<f7>" . fold-dwim-toggle)
  ;;        ("<M-f7>" . fold-dwim-hide-all)
  ;;        ("<S-M-f7>" . fold-dwim-show-all))
  )
;; This package binds no keys by default, so you need to find three
;; free and convenient key-bindings.  This is what I use:
;;
;;  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
;;  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
;;  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)
;;


;;; 
;;; origami.el
;; https://github.com/gregsexton/origami.el
(use-package origami
  :commands (origami))




















