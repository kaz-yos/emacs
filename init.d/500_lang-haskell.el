;;; Haskell-related configurations

;; Emacs for Haskell
;; http://www.haskell.org/haskellwiki/Emacs
;;
;; Using Emacs for Haskell development
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md


;;;
;;; haskell-mode.el
;; https://github.com/haskell/haskell-mode#haskell-mode-for-emacs
;; http://www.haskell.org/haskellwiki/Emacs/Installing_haskell-mode
(require 'haskell-mode)
;;
;;
(add-hook 'haskell-mode-hook
	  '(turn-on-eldoc-mode turn-on-haskell-doc turn-on-haskell-indentation))
;;
;; 3 mutually exclusive indent style
;; http://www.haskell.org/haskellwiki/Emacs/Indentation
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;
;; Keys
(defun haskell-mode-keys ()
  (local-set-key (kbd "C-c C-l") 'inferior-haskell-load-file)
  (local-set-key (kbd "C-c C-r") 'inferior-haskell-reload-file))
;;
(add-hook 'haskell-mode-hook 'haskell-mode-keys)
;;
;;; inferior haskell mode
(require 'inf-haskell)
;;
(setq haskell-program-name "/usr/bin/ghci")


;;;
;;; ghc.el
;; (require 'ghc)

