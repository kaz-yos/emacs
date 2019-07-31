;;; 500_stan-related.el ---                          -*- lexical-binding: t; -*-

;;;
;;; stan-mode.el
(use-package stan-mode
  ;; :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; These are default values.
  (setq stan-comment-start "//")
  (setq stan-comment-end "")
  (setq stan-indentation-offset 2))


;;; company-stan.el
(use-package company-stan
  ;; :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/company-stan/"
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil)
  )


;;; eldoc-stan.el
(use-package eldoc-stan
  ;; :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/eldoc-stan/"
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  )


(use-package stan-snippets
  :after stan-mode
  ;; :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/stan-snippets/"
  )

;;; ac-stan.el
(use-package ac-stan
  ;; :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/ac-stan/"
  ;; :hook (stan-mode . stan-ac-mode-setup)
  ;;
  :config
  )
