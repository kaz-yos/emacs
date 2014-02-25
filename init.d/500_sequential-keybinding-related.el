;;; Key-Chord
;; http://www.emacswiki.org/emacs/KeyChord
;; http://d.hatena.ne.jp/rubikitch/touch/20081104/1225745862
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
;;
;; view-mode
;; (key-chord-define-global "jk" 'view-mode)		; Enter read-only mode
(key-chord-define-global "jk" 'read-only-mode)		; Enter read-only mode
;;
;; highlight-sexp-mode.el
(key-chord-define-global "sx" 'highlight-sexp-mode)		; (highlight-sexp-mode)


;;; sequential-command.el for C-a C-a etc
;; Book by rubikitch p76. M-x auto-install-batch sequential-command (two files, one -config)
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)	; Define these seq-* commands. Needs sequential-command.el
(sequential-command-setup-keys)		; Rebind C-a, C-e, M-u, M-c, and M-l to seq-* commands.
(global-set-key (kbd "C-a") 'seq-home)
(global-set-key (kbd "C-e") 'seq-end)
(global-set-key (kbd "M-u") 'seq-upcase-backward-word)
(global-set-key (kbd "M-c") 'seq-capitalize-backward-word)
(global-set-key (kbd "M-l") 'seq-downcase-backward-word)


;;; smartchr.el (does not work great with multiple-cursors)
;; Similar elips: http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
;; http://ratememo.blog17.fc2.com/blog-entry-937.html
(defun my-ess-smartchr-setting ()
  (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+") (smartchr '(" + " "+")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------"))) ; test
  (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################"))) ; test
  (local-set-key (kbd "~") (smartchr '(" ~ " "~")))
  (local-set-key (kbd "$") (smartchr '("$" "$`!!'$")))
  (local-set-key (kbd "%") (smartchr '("%" " %`!!'% ")))
;;  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "["))) ; not very useful
  )
(add-hook 'ess-mode-hook	  'my-ess-smartchr-setting)
(add-hook 'inferior-ess-mode-hook 'my-ess-smartchr-setting)
;
(defun my-python-smartchr-setting ()
  (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+") (smartchr '(" + " "+")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------"))) ; test
  (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################"))) ; test
  )
(add-hook 'ein:notebook-multilang-mode-hook	'my-python-smartchr-setting)
(add-hook 'python-mode-hook			'my-python-smartchr-setting)
(add-hook 'inferior-python-mode-hook		'my-python-smartchr-setting)
;;
(defun my-LaTeX-smartchr-setting ()
  (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
  (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")))
  )
(add-hook 'LaTeX-mode-hook 'my-LaTeX-smartchr-setting)
;;
(defun my-elisp-smartchr-setting ()
  (local-set-key (kbd ";") (smartchr '("; " ";; " ";;; " ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))
  )
(add-hook 'emacs-lisp-mode-hook 'my-elisp-smartchr-setting)
