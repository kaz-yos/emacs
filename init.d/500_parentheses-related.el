;;;
;;; paredit.el
;; smartparens appears more modern. 2014-02-03
;; https://github.com/Fuco1/smartparens
;;
;; M-x install-elisp http://mumble.net/~campbell/emacs/paredit.el
(use-package paredit
  :commands (enable-paredit-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook         'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook   'enable-paredit-mode)
  (add-hook 'lisp-mode-hook               'enable-paredit-mode)
  (add-hook 'clojure-mode-hook            'enable-paredit-mode)
  (add-hook 'scheme-mode-hook             'enable-paredit-mode)
  (add-hook 'hy-mode-hook                 'enable-paredit-mode)
  ;; (add-hook 'ielm-mode-hook            'enable-paredit-mode)
  ;; paredit for ESS. too restrictive
  ;; (add-hook 'ess-mode-hook 'enable-paredit-mode)
  :config
  (setq paredit-lighter "")
  ;; No space when inserted after a word
  ;; http://stackoverflow.com/questions/913449/changing-paredit-formatting
  (defun paredit-space-for-delimiter-p (endp delimiter)
    (and (not (if endp (eobp) (bobp)))
         (memq (char-syntax (if endp (char-after) (char-before)))
               (list ?\"  ;; REMOVED ?w ?_
                     (let ((matching (matching-paren delimiter)))
                       (and matching (char-syntax matching))))))))


;;;
;;; rainbow-delimiters.el
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  ;; Activate in these modes
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook       'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook    'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook     'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook        'rainbow-delimiters-mode)
  (add-hook 'hy-mode-hook         'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook          'rainbow-delimiters-mode)
  (add-hook 'c++-mode-hook        'rainbow-delimiters-mode)
  (add-hook 'stan-mode-hook       'rainbow-delimiters-mode)
  ;;
  :config
  ;; http://ergoemacs.org/misc/emacs_rainbow-delimiters-mode.html
  ;; Just as an example
  (setq delim-test (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1))))))))))))))))
