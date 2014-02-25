;;; Python support				; external dependency
;;
;; python.el (in use):
;; http://caisah.info/emacs-for-python/
;;
;; python-mode.el (more features, more configurations):
;; http://www.janteichmann.me/projects/emacs_as_python_editor
;;
;;
;; python.el
;; http://superuser.com/questions/345595/python-el-for-long-time-python-mode-el-user
(require 'python)
;; (setq python-shell-interpreter "/usr/local/bin/python")	; symlink to python3 does not work.
;;
;; ipython setting for python.el (not used as of 2013-12-28)
(setq
 ;; python-shell-interpreter "ipython"
 python-shell-interpreter "/usr/local/bin/ipython3"
 ;; python-shell-interpreter "/usr/local/bin/ipython"
 ;; "console --pylab" required for matplotlib? 2013-12-25
 ;; http://stackoverflow.com/questions/17117074/python-shell-in-emacs-freezes-when-using-matplotlib
 ;; python-shell-interpreter-args "console --pylab"
 python-shell-interpreter-args "--pylab"	; console does not work with Python 3.3.3. 2013-12-31
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-Regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;
;; ;; Default shell interaction commands
;; (define-key map "\C-c\C-p" 'run-python)
;; (define-key map "\C-c\C-s" 'python-shell-send-string)
;; (define-key map "\C-c\C-r" 'python-shell-send-region)
;; (define-key map "\C-\M-x" 'python-shell-send-defun)
;; (define-key map "\C-c\C-c" 'python-shell-send-buffer)
;; (define-key map "\C-c\C-l" 'python-shell-send-file)
;; (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
;;
;; Redefine python-shell-send-region command to avoid sending blank line to ipython shell 2013-12-22
;; This, however, breaks the debugger. It will show the wrong lines in the beginning of the files. 2013-12-25
;; Python3's traceback is smarter and correctly shows the error with the simplified send-region. 2014-01-02
(defun python-shell-send-region (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (python-shell-send-string
   (buffer-substring start end)
   ;; No need to send blank lines in ipython? 2013-12-22
   ;; (concat
   ;;  (let ((line-num (line-number-at-pos start)))
   ;;    ;; When sending a region, add blank lines for non sent code so
   ;;    ;; backtraces remain correct.
   ;;    (make-string (1- line-num) ?\n))
   ;;  (buffer-substring start end))
   nil t))
;;
;; jedi.el	; Python auto-completion for Emacs
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
;;
;; Code to emulate ESS/R behavior 2013-12-22 version
(defun my-python-start ()
  (interactive)
  (if (not (member "*Python*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))	; Split into two windows
	(call-interactively 'run-python)	; Activate Python if not running (runs ipython)
        (set-window-buffer w1 "*Python*")	; Python on the left (w1)
        (set-window-buffer w2 w1name)		; Script on the right (w2)
	(select-window w2)			; Select script (w2) Added
	)))
;; Start python if not started. Send region if selected, line if not selected (whole def if it is def)
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
(defun my-python-eval ()
  (interactive)
  (my-python-start)
  (if (and transient-mark-mode mark-active)			; Check if selection is present
      (python-shell-send-region (point) (mark))			; If selected, send region
    ;; If not selected, do all the following
    (beginning-of-line)						; Move to the beginning of line
    (if (looking-at "def")					; Check if the first word is def (function def)
	(progn							; If it is def
	  (python-shell-send-defun ())				; Send whole def
	  (python-nav-end-of-defun)				; Move to the end of def
	  (python-nav-forward-statement)			; Move to the next statement
	  )
      ;; If it is not def, do all the following
      (python-shell-send-region (point-at-bol) (point-at-eol))	; Send the current line
      (python-nav-forward-statement)				; Move to the next statement
      )
    ;; Activate shell window, and switch back
    (progn
      (setq w-script (selected-window))				; Remeber the script window
      (python-shell-switch-to-shell)				; Switch to the shell
      (select-window w-script)					; Switch back to the script window
      )
    ))
;;
;; Define hooks
(add-hook 'python-mode-hook		; For Python script
          '(lambda()
	     (local-set-key (kbd "<S-return>") 'my-python-eval)
	     (local-set-key (kbd "<C-return>") 'my-python-eval)
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
;;
;;
;; ein.el	; Emacs IPython Notebook (EIN) ; Current version does not work with ipython 2.0.0 as of 2013-12-20
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
;;
;; ;; elpy.el	; python.el replacement. No need for now. 2013-12-22
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
