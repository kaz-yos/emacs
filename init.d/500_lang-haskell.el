;;; Haskell-related configurations

;; Emacs for Haskell
;; http://www.haskell.org/haskellwiki/Emacs
;;
;; Using Emacs for Haskell development
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md


;;;
;;; haskell-mode.el
;; https://github.com/haskell/haskell-mode#haskell-mode-for-emacs
(require 'haskell-mode)
;;
;;
(add-hook 'haskell-mode-hook
	  '(turn-on-eldoc-mode turn-on-haskell-doc turn-on-haskell-indent))


;;;
;;; ghc.el
(require 'ghc)

