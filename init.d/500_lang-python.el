;;; PYTHON SUPPORT-RELATED
;;
;; python.el (in use):
;; http://caisah.info/emacs-for-python/
;;
;; python-mode.el (more features, more configurations):
;; http://www.janteichmann.me/projects/emacs_as_python_editor


;;;
;;; python.el
;; http://superuser.com/questions/345595/python-el-for-long-time-python-mode-el-user
(require 'python)
;;
;; Use python3 from homebrew.
;; (setq python-shell-interpreter "/usr/local/bin/python3")
(setq python-shell-interpreter "/anaconda/bin/python")
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
(add-hook 'python-mode-hook		; For Python script
          '(lambda()
	     ;; (local-set-key (kbd "<C-return>") 'my-python-eval)
	     (local-unset-key (kbd "DEL"))	; Disable python-indent-dedent-line-backspace
	     ;; (eldoc-mode 1)			; eldoc in the mode line. Slow? 2013-12-25
	     (local-set-key (kbd "C-m") 'newline-and-indent)	; Indent after newline
	     (local-set-key (kbd "M-p") 'ess-nuke-trailing-whitespace)
	     (local-set-key (kbd "C-c c") 'python-check)	; Check grammar
	     ))
;;
(add-hook 'inferior-python-mode-hook	; For Python process
          '(lambda()
             ;; (local-set-key (kbd "C-<up>") 'comint-previous-input)
             ;; (local-set-key (kbd "C-<down>") 'comint-next-input)
	     ))


;;;
;;; jedi.el	; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/
(setq jedi:complete-on-dot t)				; binds . to jedi:dot-complete. Dot alone activates jedi
(add-hook 'python-mode-hook	     'jedi:setup)	; for full setup
(add-hook 'inferior-python-mode-hook 'jedi:setup)	; for full setup
;; (add-hook 'python-mode-hook	     'jedi:ac-setup)	; for completion only. keys are not changed.
;; (add-hook 'inferior-python-mode-hook 'jedi:ac-setup)	; for completion only. keys are not changed.
;;
;; C-c		Prefix Command
;; .		jedi:dot-complete
;; <C-M-tab>	jedi:complete
;; <C-tab>		other-window-or-split
;; C-c ,		jedi:goto-definition-pop-marker
;; C-c .		jedi:goto-definition
;; C-c /		helm-jedi-related-names
;; C-c ?		jedi:show-doc
;;
(add-hook 'jedi-mode-hook
          '(lambda()
	     ;; (local-set-key (kbd "<C-M-tab>") 'jedi:complete)	; Assigned to Python major mode
	     (define-key jedi-mode-map (kbd "<C-tab>") 'other-window-or-split)	; Assigned to Jedi minor mode
	     (define-key jedi-mode-map (kbd "<C-M-tab>") 'jedi:complete)	; Assigned to Jedi minor mode
	     ;; jedi:show-doc
	     (define-key jedi-mode-map (kbd "C-c C-v") 'jedi:show-doc)		; Simulate ESS
	     ))


;;;
;;; ein.el	; Emacs IPython Notebook (EIN)
;; Current version does not work with ipython 2.0.0 as of 2013-12-20
;; 20140317.1114 did not work with ipython 2.0.0 stable as of 2014-04-08
;;
;; This is fundamentally different from python.el and can coexist. 2013-12-22
;; http://tkf.github.com/emacs-ipython-notebook/
;; Usage
;; Start IPython notebook server with $ ipython notebook --pylab inline
;; Hit M-x ein:notebooklist-open to open notebook list.
(require 'ein)
;; Auto complete for ein
(setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
;; (setq ein:use-auto-complete-superpack t)
(add-hook 'ein:notebook-multilang-mode-hook	; For EIN
          '(lambda()
             (local-set-key (kbd "<C-return>") 'ein:worksheet-execute-cell)
             (local-set-key (kbd "<S-return>") 'ein:worksheet-execute-cell)
	     (local-set-key (kbd "C-c a")      'ein:worksheet-insert-cell-above)
	     (local-set-key (kbd "C-c b")      'ein:worksheet-insert-cell-below)
	     (local-set-key (kbd "C-c k")      'ein:worksheet-kill-cell)
	     (local-set-key (kbd "C-c w")      'ein:worksheet-copy-cell)
	     (local-set-key (kbd "C-c y")      'ein:worksheet-yank-cell)
	     (local-set-key (kbd "C-c p")      'ein:worksheet-goto-prev-input)
	     (local-set-key (kbd "C-c n")      'ein:worksheet-goto-next-input)
	     ))


;;;
;;; elpy.el	; python.el replacement. No need for now. 2013-12-22
;; ;; https://github.com/jorgenschaefer/elpy/wiki
;; ;; Need to install elpy/rope/jedi/flake8 via $ sudo pip install
;; ;; $ sudo pip install --upgrade elpy # to upgrade to the latest elpy
;; ;; $ sudo ln -s /usr/local/bin/flake8 /usr/bin/flake8 # To make it visible to emacs. 2013-12-22
;; ;;
;; ;; (package-initialize)
;; ;; To use elpy
;; (elpy-enable)
;; ;; To use ipython	; 2013-12-22 broken? ipython not found
;; (elpy-use-ipython)
;; ;; Simplify modeline
;; (elpy-clean-modeline)
;; ;; jedi as completion backend
;; (setq elpy-rpc-backend "jedi")
;; ;;
;; ;; Fix yas-snippet-dirs (elpy breaks configuration)	; Not necessary as of 2013-12-22
;; ;; (setq yas-snippet-dirs
;; ;;       '("~/.emacs.d/snippets"
;; ;; 	"/Users/kazuki/.emacs.d/el-get/yasnippet/snippets"
;; ;; 	))
