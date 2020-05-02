;;;
;;; PYTHON SUPPORT-RELATED

;;;  python.el
;; http://superuser.com/questions/345595/python-el-for-long-time-python-mode-el-user
(use-package python
  :commands (python-mode)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)
  :bind (:map python-mode-map
              ("C-m" . newline-and-indent)
              ("C-c c" . python-check))
  ;;
  :config
  (setq python-shell-interpreter (executable-find "python"))
  ;; Disable python-indent-dedent-line-backspace
  (unbind-key "DEl" python-mode-map))


;;;  anaconda-mode.el
;; Code navigation, documentation lookup and completion for Python.
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :commands (anaconda-mode)
  :hook ((python-mode . anaconda-mode)
         ;; anaconda-eldoc-mode provide document function to eldoc-mode.
         (python-mode . anaconda-eldoc-mode)))


;;;  company-anaconda.el
;; https://github.com/proofit404/company-anaconda
(use-package company-anaconda
  :commands (company-anaconda-setup)
  :hook (python-mode . company-anaconda-setup)
  ;;
  :config
  (defun company-anaconda-setup ()
    "Add company-anaconda to company-backends buffer-locally."
    (add-to-list (make-local-variable 'company-backends)
                 ;; If you want to see anaconda-mode completions together with ones comes from inferior python process use company grouped backend instead:
                 '(company-anaconda :with company-capf))))


;;;  ein.el
;; https://github.com/millejoh/emacs-ipython-notebook
;; http://millejoh.github.io/emacs-ipython-notebook/
;; https://github.com/millejoh/emacs-ipython-notebook#requirements
;; http://millejoh.github.io/emacs-ipython-notebook/#usage
;; https://github.com/millejoh/emacs-ipython-notebook/wiki/Tips
;;
;; ein:notebooklist-open fails #154 (token support)
;; https://github.com/millejoh/emacs-ipython-notebook/issues/154
;; https://github.com/millejoh/emacs-ipython-notebook#requirements
;;
;; Anaconda managing environments
;; https://conda.io/docs/user-guide/tasks/manage-environments.html
;;
;; Usage
;; M-x shell
;; $ conda env list
;; $ source activate someenvironmentname
;; M-x ein:notebooklist-login (enter the token as the password)
;; M-x ein:notebooklist-open
(use-package ein
  :ensure t
  :commands (;; Start the jupyter notebook server at the given path.
             ;; This only works if jupyter is in the default conda env.
             ein:jupyter-server-start
             ;; Log in and open a notebooklist buffer for a running jupyter notebook server.
             ein:jupyter-server-login-and-open
             ;; Log on to a jupyterhub server using PAM authentication.
             ;; Requires jupyterhub version 0.8 or greater.
             ein:jupyterhub-connect
             ;;
             ;; Login to IPython notebook server.
             ;; Use the server token as a password.
             ein:notebooklist-login
             ;; Open notebook list buffer.
             ein:notebooklist-open)
  ;;
  :config
  ;; Use auto-complete for ein
  (add-hook 'ein:notebook-multilang-mode-hook '(lambda () (company-mode -1)))
  (add-hook 'ein:notebook-multilang-mode-hook '(lambda () (auto-complete-mode -1)))
  ;; http://millejoh.github.io/emacs-ipython-notebook/#quick-try
  (use-package ein-loaddefs)
  ;;
  (use-package ein-notebook
    :config
    ;; Sets the frequency (in seconds) at which the notebook is automatically saved.
    (setq ein:notebook-autosave-frequency 10))
  ;;
  (use-package ein-subpackages)
  ;;
  (use-package ein-jupyter
    :config
    ;; The name of the buffer to run a jupyter notebook server session in.
    (setq ein:jupyter-server-buffer-name "*ein:jupyter-server*")
    ;; The default command to start a jupyter notebook server.
    (setq ein:jupyter-default-server-command "jupyter")
    ;; options you wish to include with the call to the jupyter notebook.
    (setq ein:jupyter-server-args nil)
    ;;
    (setq ein:jupyter-default-notebook-directory nil)))


;;;
;;; hy-mode.el
;; https://github.com/hylang/hy-mode
;; http://docs.hylang.org/en/stable/
(use-package hy-mode
  :ensure t
  :mode "\\.hy"
  :commands (company-hy-setup)
  :hook (hy . company-hy-setup)
  ;;
  :config
  (setq hy-mode-inferior-lisp-command (executable-find "hy"))
  ;;
  ;; Define company backend
  (defun company-hy-setup ()
    "Add company-hy to company-backends buffer-locally."
    (add-to-list (make-local-variable 'company-backends)
                 'company-hy)))
