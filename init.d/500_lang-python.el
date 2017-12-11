;;;
;;; PYTHON SUPPORT-RELATED

;;;  python.el
;; http://superuser.com/questions/345595/python-el-for-long-time-python-mode-el-user
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)
  ;;
  :config
  (setq python-shell-interpreter (executable-find "python"))
  ;;
  ;; ;; Default shell interaction commands
  ;; (define-key map (kbd "C-c C-p") 'run-python)
  ;; (define-key map (kbd "C-c C-s") 'python-shell-send-string)
  ;; (define-key map (kbd "C-c C-r") 'python-shell-send-region)
  ;; (define-key map (kbd "C-M-x")   'python-shell-send-defun)
  ;; (define-key map (kbd "C-c C-c") 'python-shell-send-buffer)
  ;; (define-key map (kbd "C-c C-l") 'python-shell-send-file)
  ;; (define-key map (kbd "C-c C-z") 'python-shell-switch-to-shell)
  ;;
  ;;
  ;; Define hooks
  ;; For Python script
  (add-hook 'python-mode-hook
            '(lambda()
               ;; Disable python-indent-dedent-line-backspace
               (local-unset-key (kbd "DEL"))
               ;; eldoc in the mode line. Slow? 2013-12-25
               ;; (eldoc-mode 1)
               ;; Indent after newline
               (local-set-key (kbd "C-m") 'newline-and-indent)
               ;; Check grammar
               (local-set-key (kbd "C-c c") 'python-check))))


;;;  ein.el
(use-package ein-loaddefs
  :commands (ein:jupyter-server-start
             ein:jupyter-server-login-and-open
             ein:jupyterhub-connect
             ;; This is for old iPython Notebook
             ein:connect-to-notebook)
  :config
  (setq ein:notebook-autosave-frequency 10))



;;;  hy-mode.el
;; https://github.com/hylang/hy-mode
;; http://docs.hylang.org/en/stable/
(use-package hy-mode
  :mode "\\.hy"
  :config
  (setq hy-mode-inferior-lisp-command (executable-find "hy"))
  ;; Activate auto-complete-mode
  (add-hook 'hy-mode-hook
            '(lambda ()
               (auto-complete-mode 1))))
