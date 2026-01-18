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


;;;
;;; IRONY-RELATED
;;;  irony-mode.el
;; https://github.com/Sarcasm/irony-mode
;; https://github.com/Sarcasm/irony-mode/wiki/Mac-OS-X-issues-and-workaround
;; https://github.com/Sarcasm/irony-mode/issues/351
;; https://github.com/Golevka/emacs-clang-complete-async/issues/18
;;
;; Install llvm with header files.
;; brew install --with-clang --all-targets --rtti --universal --jit llvm
;;
;; M-x irony-install-server
;; Add the following -DLIBCLANG* options for brew's llvm.
;; -DLIBCLANG_INCLUDE_DIR\=/usr/local/opt/llvm/include/ -DLIBCLANG_LIBRARY\=/usr/local/opt/llvm/lib/libclang.dylib
;;
;; How to selectively remove entries from the CMake cache from the command line
;; https://blogs.kde.org/2011/02/05/how-selectively-remove-entries-cmake-cache-command-line
;;
;; ~/.emacs.d/irony/bin/irony-server is installed.
(use-package irony
  :ensure t
  :commands (irony-mode)
  ;;
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)))
;;
;;;  company-irony.el
;; https://github.com/Sarcasm/company-irony/
(use-package company-irony
  :ensure t
  :commands (company-irony
             company-irony-setup)
  :hook ((c++-mode . company-irony-setup)
         (c-mode . company-irony-setup)
         (objc-mode . company-irony-setup))
  :config
  (defun company-irony-setup ()
    "Add company-irony to company-backends buffer-locally."
    (add-to-list (make-local-variable 'company-backends)
                 'company-irony)))
;;
;;;  irony-eldoc.el
(use-package irony-eldoc
  :ensure t
  :commands (irony-eldoc)
  :hook (irony-mode . irony-eldoc))
