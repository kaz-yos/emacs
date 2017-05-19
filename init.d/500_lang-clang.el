;;; 500_lang-clang.el ---                            -*- lexical-binding: t; -*-

;; using the current buffer's file name in M-x compile
;; http://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile
(add-hook 'c++-mode-hook
          (lambda ()
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
                       "-lm"
                       (file-name-sans-extension buffer-file-name))))))
