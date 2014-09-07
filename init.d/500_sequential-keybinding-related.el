;;; Sequential key binding related

;;;
;;; Key-Chord
;; http://www.emacswiki.org/emacs/KeyChord
;; http://d.hatena.ne.jp/rubikitch/touch/20081104/1225745862
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
;;
;; Toggle read-only status
(key-chord-define-global "jk" 'toggle-read-only)
;;
;; highlight-sexp-mode.el
(key-chord-define-global "sx" 'highlight-sexp-mode)		; (highlight-sexp-mode)


;;;
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


;;;
;;; smartchr.el (does not work great with multiple-cursors)
;; 2014-03-30 (mc/prompt-for-inclusion-in-whitelist 'smartchr) did not help.
;; Similar elips: http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
;;
;; ESS
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
;;
;; Python
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
;; LaTeX
(defun my-LaTeX-smartchr-setting ()
  (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
  (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")))
  )
(add-hook 'LaTeX-mode-hook 'my-LaTeX-smartchr-setting)
;;
;; Emacs Lisp
(defun my-elisp-smartchr-setting ()
  (local-set-key (kbd ";") (smartchr '("; " ";; " ";;; " ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))
  )
(add-hook 'emacs-lisp-mode-hook 'my-elisp-smartchr-setting)
;;
;; Haskell
(defun my-haskell-smartchr-setting ()
  (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+") (smartchr '(" + " "+")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------")))
  )
(add-hook 'haskell-mode-hook 'my-haskell-smartchr-setting)


;;;
;;; smartrep.el
;; http://sheephead.homelinux.org/2011/12/19/6930/
(require 'smartrep)
;;
;; org-mode
(eval-after-load "org"
        '(progn
           (smartrep-define-key
            org-mode-map "C-c" '(("C-n" . (lambda ()
                                            (outline-next-visible-heading 1)))
                                 ("C-p" . (lambda ()
                                            (outline-previous-visible-heading 1)))))))
;; Make C-' a prefix
(defvar ctl-quote-map (make-keymap))
(define-key global-map (kbd "C-'") ctl-quote-map)
;; Scroll the other window
(smartrep-define-key
 global-map "C-'" '(("n" . (lambda () (scroll-other-window 1)))
                    ("p" . (lambda () (scroll-other-window -1)))
                    ("N" . 'scroll-other-window)
                    ("P" . (lambda () (scroll-other-window '-)))
                    ("a" . (lambda () (beginning-of-buffer-other-window 0)))
                    ("e" . (lambda () (end-of-buffer-other-window 0)))))
;;
