;;; 500_stan-related.el ---                          -*- lexical-binding: t; -*-

;;;
;;; stan-mode.el
(use-package stan-mode
  :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :commands (my-stan-setup)
  :hook (stan-mode . my-stan-setup)
  ;;
  :config
  ;; These variables are not directly used.
  (setq stan-comment-start "//")
  (setq stan-comment-end "")
  ;; Rather, they have to be set to buffer-local comment-start/end variables.
  (defun my-stan-setup ()
    (setq comment-start stan-comment-start)
    (setq comment-end stan-comment-end)
    ;; Two-character indent.
    ;; https://mc-stan.org/docs/2_18/stan-users-guide/white-space.html
    (setq c-basic-offset 2)))


;;; company-stan.el
(use-package company-stan
  :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/company-stan/"
  :commands (company-stan-backend
             company-stan-setup)
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  (defun company-stan-setup ()
    "Set up `company-stan-backend'

Add `company-stan-backend' to `company-backends'
buffer locally."
    ;; Add company-stan-backend to the company-backends buffer-locally.
    (add-to-list (make-local-variable 'company-backends)
                 'company-stan-backend)))


;;; eldoc-stan.el
(use-package eldoc-stan
  :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/eldoc-stan/"
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  (defun eldoc-stan-setup ()
    "Set up `eldoc-stan-eldoc-documentation-function'

Specify `eldoc-stan-eldoc-documentation-function' as
`eldoc-documentation-function'"
    (setq-local eldoc-documentation-function
                #'eldoc-stan-eldoc-documentation-function)))
