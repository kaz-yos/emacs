;;;
;;; paredit.el
;; smartparens appears more modern. 2014-02-03
;; https://github.com/Fuco1/smartparens
;;
;; M-x install-elisp http://mumble.net/~campbell/emacs/paredit.el
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook         'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook   'enable-paredit-mode)
(add-hook 'lisp-mode-hook               'enable-paredit-mode)
(add-hook 'clojure-mode-hook            'enable-paredit-mode)
(add-hook 'scheme-mode-hook             'enable-paredit-mode)
(add-hook 'hy-mode-hook                 'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook            'enable-paredit-mode)
;; (add-hook 'ess-mode-hook 'enable-paredit-mode)		; paredit for ESS. too restrictive
;; No space when inserted after a word
;; http://stackoverflow.com/questions/913449/changing-paredit-formatting
(defun paredit-space-for-delimiter-p (endp delimiter)
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\"  ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))
;;; paredit-menu.el
;; Adds a menu to paredit.el as memory aid
(require 'paredit-menu)


;;;
;;; rainbow-delimiters.el
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
;;
;; Activate in these modes
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook       'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook    'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook     'rainbow-delimiters-mode)
(add-hook 'ess-mode-hook        'rainbow-delimiters-mode)
(add-hook 'hy-mode-hook         'rainbow-delimiters-mode)
;;
;; http://ergoemacs.org/misc/emacs_rainbow-delimiters-mode.html
;; Configure in init-customize.el
(setq delim-test (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1)))))))))))))))


;;;
;;; smartparens.el
;; https://github.com/Fuco1/smartparens/wiki
;; paredit and smartparens: https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens
(require 'smartparens-config)
