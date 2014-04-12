;;; Python support				; external dependency
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
(setq python-shell-interpreter "/usr/local/bin/python3")
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
;; Redefine python-shell-send-region command to avoid sending blank line to ipython shell 2013-12-22
;; This, however, breaks the debugger. It will show the wrong lines in the beginning of the files. 2013-12-25
;; Python3's traceback is smarter and correctly shows the error with the simplified send-region. 2014-01-02
'(defun python-shell-send-region (start end)
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
;;;
;;; my-send-to-python
(defun my-send-to-python (start end)
    "Sends expression to *Python* and have it evaluated."

  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change to cider REPL
    (python-shell-switch-to-shell)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (comint-send-input)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;; my-python-eval
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
(defun my-python-eval ()
  "Evaluates Python expressions"
  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; defined in 200_my-misc-functions-and-bindings.el
    (my-repl-start "*Python*" #'(lambda () (call-interactively 'run-python)))

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(progn
	  ;; Send region
	  (python-shell-send-region (point) (mark))
	  ;; If the last letter is not \n, send \n
	  (if (not (equal (substring (buffer-substring-no-properties (point) (mark)) -1) "\n"))
	      (python-shell-send-string "\n")))
      
      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Check if the first word is def (function def)
      (if (looking-at "def ")
	  ;; If it is def
	  (progn
	    ;; Send whole def
	    (python-shell-send-defun ())
	    ;; Move to the end of def
	    (python-nav-end-of-defun)
	    ;; Move to the next statement
	    (python-nav-forward-statement)			
	    )
	;; If it is not def, do all the following
	;; Set mark at current position
	(set-mark (point))
	;; Go to the end of statment
	(python-nav-end-of-statement)
	;; Go to the end of block
	(python-nav-end-of-block)
	;; Maker transient region active
	(setq mark-active t)
	;; Send region
	(python-shell-send-region (point) (mark))
	;; If the last letter is not \n, send \n
	(if (not (equal (substring (buffer-substring-no-properties (point) (mark)) -1) "\n"))
	    (python-shell-send-string "\n"))
	;; Move to the next statement
	(python-nav-forward-statement)				
	)
      ;; Activate shell window, and switch back
      (progn
	;; Remeber the script window
	(setq w-script (selected-window))
	;; Switch to the shell
	(python-shell-switch-to-shell)
	;; Switch back to the script window
	(select-window w-script)					
	)
      )))
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
