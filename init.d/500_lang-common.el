;;; 500_lang-common.el ---                           -*- lexical-binding: t; -*-
;; Settings useful for programming in general

;;;
;;; column-marker.el
;; http://www.emacswiki.org/emacs/column-marker.el
(use-package column-marker
  :commands column-marker80
  :config
  (defun column-marker80 ()
    (interactive)
    (column-marker-2 80)))


;;;
;;; aggressive-indent.el
;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :commands (aggressive-indent-mode))
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)


;;;
;;; pos-tip.el
;; https://github.com/tjarvstrand/pos-tip
;; It breaks down on macOS?
(use-package pos-tip
  :disabled t
  :config
  (defun pos-tip-eldoc-display-message (format-string &rest args)
    "Display eldoc message near point."
    (when format-string
      (pos-tip-show (apply 'format format-string args))))
  ;; Use
  (setq eldoc-message-function #'pos-tip-eldoc-display-message))


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


;;;
;;; helm-gtags.el
;; https://github.com/syohex/emacs-helm-gtags
;; http://tuhdo.github.io/c-ide.html
;; $ brew install global # GNU GLOBAL
(use-package helm-gtags
  :commands (helm-gtags-mode)
  ;;
  :init
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-pulse-at-cursor t)
  (setq helm-gtags-prefix-key "\C-cg")
  (setq helm-gtags-suggested-key-mapping t)
  ;;
  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  ;;
  :config
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))
