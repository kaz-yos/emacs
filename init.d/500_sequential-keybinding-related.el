;;; Sequential key binding related

;;;
;;; sequential-command.el for C-a C-a etc
;; Book by rubikitch p76. M-x auto-install-batch sequential-command (two files, one -config)
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(use-package sequential-command
  :config
  (require 'sequential-command-config)
  (sequential-command-setup-keys)
  ;; (global-set-key (kbd "C-a") 'seq-home)
  ;; (global-set-key (kbd "C-e") 'seq-end)
  ;; (global-set-key (kbd "M-u") 'seq-upcase-backward-word)
  ;; (global-set-key (kbd "M-c") 'seq-capitalize-backward-word)
  ;; (global-set-key (kbd "M-l") 'seq-downcase-backward-word)
  )


;;;
;;; electric-operator.el
;; https://github.com/davidshepherd7/electric-operator
;; http://rubikitch.com/tag/emacs-key-combo/
(use-package electric-operator
  :disabled t
  :config
  (electric-operator-add-rules-for-mode
   'ess-mode
   (cons "#" "# ")
   (cons "##" "## ")
   (cons "###" "### ")
   (cons "####" "################################################################################"))
  ;;
  ;; hooks
  (add-hook 'ess-mode-hook #'electric-operator-mode))


;;;
;;; smartchr.el
;; https://github.com/imakado/emacs-smartchr
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
;;
;; Problem with multiple-cursors.el
;; 2014-03-30 (mc/prompt-for-inclusion-in-whitelist 'smartchr) did not help.
;;
(use-package smartchr
  :init
;;;  ESS
  (defun smartchr-ess-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
    (local-set-key (kbd "+") (smartchr '("+" " + ")))
    (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------")))
    (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################")))
    (local-set-key (kbd "~") (smartchr '("~" " ~ ")))
    (local-set-key (kbd "$") (smartchr '("$" "$`!!'$")))
    (local-set-key (kbd "%") (smartchr '("%" " %`!!'% ")))
    )
  (add-hook 'ess-mode-hook          'smartchr-ess-mode-set)
  (add-hook 'inferior-ess-mode-hook 'smartchr-ess-mode-set)
  ;;
;;;  Python
  (defun smartchr-python-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
    (local-set-key (kbd "+") (smartchr '("+" " + ")))
    (local-set-key (kbd "-") (smartchr '("-" " - ")))
    (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################"))))
  (add-hook 'ein:notebook-multilang-mode-hook 'smartchr-python-mode-set)
  (add-hook 'python-mode-hook                 'smartchr-python-mode-set)
  (add-hook 'inferior-python-mode-hook        'smartchr-python-mode-set)
  ;;
;;;  SML
  (defun smartchr-sml-mode-set ()
    (local-set-key (kbd "=") (smartchr '(" = " " => " "=")))
    (local-set-key (kbd ":") (smartchr '(" : " "::"))))
  (add-hook 'sml-mode-hook 'smartchr-sml-mode-set)
  ;;
;;;  LaTeX
  (defun smartchr-LaTeX-mode-set ()
    (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
    (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))))
  (add-hook 'LaTeX-mode-hook 'smartchr-LaTeX-mode-set)
  ;;
;;;  Org
  (defun smartchr-org-mode-set ()
    (local-set-key (kbd "^") (smartchr '("^{`!!'}" "^")))
    (local-set-key (kbd "_") (smartchr '("_{`!!'}" "_" "_`!!'_")))
    (local-set-key (kbd "*") (smartchr '("*" "*`!!'*")))
    (local-set-key (kbd "/") (smartchr '("/" "/`!!'/")))
    (local-set-key (kbd "=") (smartchr '("=" "=`!!'=")))
    (local-set-key (kbd "-") (smartchr '("-" "-`!!'-")))
    (local-set-key (kbd "+") (smartchr '("+" "+`!!'+")))
    (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
    (local-set-key (kbd "#") (smartchr '("# " "#+" "# ############################################################################ #")))
    (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))))
  (add-hook 'org-mode-hook 'smartchr-org-mode-set)
  ;;
  ;; Emacs Lisp
  (defun my-elisp-smartchr-setting ()
;;;  Emacs Lisp
  (defun smartchr-emacs-lisp-mode-set ()
    (local-set-key (kbd ";") (smartchr '("; " ";; " ";;; " ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"))))
  (add-hook 'emacs-lisp-mode-hook 'smartchr-emacs-lisp-mode-set)
  ;;
;;;  Haskell
  (defun smartchr-haskell-mode-set ()
    (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
    (local-set-key (kbd "+") (smartchr '(" + " " ++ " "+")))
    (local-set-key (kbd "-") (smartchr '(" - " " -> " "-")))
    (local-set-key (kbd ":") (smartchr '(" : " " :: " ":"))))
  (add-hook 'haskell-mode-hook 'smartchr-haskell-mode-set)
  ;;
;;;  Ruby
  (defun smartchr-ruby-mode-set ()
    (local-set-key (kbd "=") (smartchr '(" = " " == " "="))))
  (add-hook 'ruby-mode-hook 'smartchr-ruby-mode-set)
  ;;
  ;; Configuration
  :commands (smartchr))


;;;
;;; key-combo.el
;; https://github.com/uk-ar/key-combo
;; http://d.hatena.ne.jp/uk-ar/20111208/1322572618
;; http://qiita.com/akisute3@github/items/0141c92dca0992732af8
;; http://fnwiya.hatenablog.com/entry/2015/11/22/004358
;;
(use-package key-combo
  :disabled t
  :config
  ;; ESS
  (setq my-ess-mode-hooks
        '(ess-mode-hook
          inferior-ess-mode-hook))
  (setq my-key-combos-for-ess
        '(("=" . ("=" " = " " == "))
          ("+" . ("+" " + "))
          ("-" . ("-" " - " "--------------------------------------------------------------------------------"))
          ("#" . ("# " "## " "### " "################################################################################"))
          ("~" . ("~" " ~ "))
          ("$" . ("$" "$`!!'$"))
          ("%" . ("%" " %`!!'% "))))
  (key-combo-define-hook my-ess-mode-hooks
                         'my-ess-mode-hooks
                         my-key-combos-for-ess)
  ;;
  ;; Python
  (setq my-python-mode-hooks
        '(ein:notebook-multilang-mode-hook
          python-mode-hook
          inferior-python-mode-hook))
  (setq my-key-combos-for-python
        '(("=" . ("=" " = " " == "))
          ("+" . ("+" " + "))
          ("-" . ("-" " - "))
          ("#" . ("# " "## " "### " "################################################################################"))))
  (key-combo-define-hook my-python-mode-hooks
                         'my-python-mode-hooks
                         my-key-combos-for-python)
  ;;
  ;; LaTeX
  (setq my-latex-mode-hooks
        '(LaTeX-mode-hook))
  (setq my-key-combos-for-latex
        '(("$" . ("$`!!'$" "$"))
          ("%" . ("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))))
  (key-combo-define-hook my-latex-mode-hooks
                         'my-latex-mode-hooks
                         my-key-combos-for-latex))


;;;
;;; smartrep.el
;; http://sheephead.homelinux.org/2011/12/19/6930/
(use-package smartrep
  :config
  ;; org-mode
  (eval-after-load "org"
    '(progn
       (smartrep-define-key
           org-mode-map "C-c" '(("C-n" . (lambda ()
                                           (outline-next-visible-heading 1)))
                                ("C-p" . (lambda ()
                                           (outline-previous-visible-heading 1)))))))
  ;;
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
                         ("e" . (lambda () (end-of-buffer-other-window 0))))))
