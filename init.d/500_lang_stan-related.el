;;; 500_stan-related.el ---                          -*- lexical-binding: t; -*-


;; Uncomment the line below if not required elsewhere.
;; (require 'use-package)

;;; stan-mode.el
(use-package stan-mode
  ;; :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  ;; :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/stan-mode/company-stan/"
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  ;; :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/stan-mode/eldoc-stan/"
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/stan-mode/flycheck-stan/"
  :hook (stan-mode . flycheck-stan-stanc3-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; stan-snippets.el
(use-package stan-snippets
  ;; :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/stan-mode/stan-snippets/"
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; ac-stan.el
(use-package ac-stan
  ;; :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/stan-mode/ac-stan/"
  ;; Delete the line below if using.
  :disabled t
  :hook (stan-mode . stan-ac-mode-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )
