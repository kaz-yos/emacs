;;; 500_lang-clang.el ---                            -*- lexical-binding: t; -*-

;;;
;;; cc-mode.el
(use-package cc-mode
  :config
  ;; Indentation
  (setq c-basic-offset 4)
  ;;
  ;; Keys
  (define-key c++-mode-map (kbd "A-c") #'save-all-and-compile)
  (define-key c++-mode-map (kbd "A-s") #'save-all-and-compile)
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
;;; company-c-headers.el
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :commands (company-c-headers)
  :init
  (add-to-list 'company-backends 'company-c-headers))
