;;; 500_lang-clang.el ---                            -*- lexical-binding: t; -*-

;;;
;;; cc-mode.el
(use-package cc-mode
  :bind (:map c++-mode-map
              ("A-s" . save-all-and-compile)
              ("M-s M-s" . save-all-and-compile))
  :config
  ;; Indentation by two
  ;; https://mc-stan.org/docs/2_18/stan-users-guide/white-space.html
  (setq-default c-basic-offset 2)
  ;;
  ;; using the current buffer's file name in M-x compile
  ;; http://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile
  (defun c++compile-command-setter ()
    "Set compile-command for C++"
    (unless (or (file-exists-p "makefile")
                (file-exists-p "Makefile"))
      ;; Proceed unless there is a Makefile.
      (set
       ;; (make-local-variable VARIABLE)
       ;; Make VARIABLE have a separate value in the current buffer.
       (make-local-variable 'compile-command)
       ;; This will be the value for c++.
       (concat "g++ -pipe -O2 -std=c++14 "
               buffer-file-name
               " -lm -o "
               (file-name-sans-extension buffer-file-name)))))
  (add-hook 'c++-mode-hook #'c++compile-command-setter))


;;;
;;; clang-format.el
;; https://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :ensure t
  :bind (:map c++-mode-map
         ("C-c f" . clang-format-buffer))
  :config)


;;;
;;; c-eldoc.el
;; MELPA version
;; https://github.com/nflath/c-eldoc
;; Version enhanced with deferred.el
;; https://github.com/mooz/c-eldoc
(use-package c-eldoc
  :ensure t
  :commands (c-eldoc-format-arguments-string))


;;;
;;; company-c-headers.el
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :ensure t
  :commands (company-c-headers
             company-c-headers-setup)
  :hook ((c++-mode . company-c-headers-setup)
         (c-mode . company-c-headers-setup)
         (objc-mode . company-c-headers-setup))
  :config
  (defun company-c-headers-setup ()
    "Add company-c-headers to company-backends buffer-locally."
    (add-to-list (make-local-variable 'company-backends)
                 'company-c-headers)))
