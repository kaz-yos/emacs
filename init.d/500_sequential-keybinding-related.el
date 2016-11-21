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
  ;; Use init as they have to be present at the time of hook activation by major modes
  :init
;;; Define multiple-cursors work around functions
  (defun smartchr-construct-unsetter (smartchar-set-function)
    "Generate an unsetter lambda from a smartchr setter function"
    ;; Extract instructions in the body of the setterr
    ;; http://endlessparentheses.com/new-in-emacs-25-1-more-flow-control-macros.html
    (let ((lst-instr (thread-first smartchar-set-function
                       ;; Extract symbol’s function definition
                       symbol-function
                       ;; Drop first two elements (lambda nil)
                       cddr)))
      ;; manipulate list elements to create an unsetter
      (thread-last (mapcar
                    ;; Loop over instructions generating unset instructions
                    (lambda (elt) (cons 'local-unset-key (list (nth 1 elt))))
                    lst-instr)
        ;; Add an empty argument part
        (cons nil)
        ;; Add lambda to generate an anonymous function
        (cons 'lambda))))
  ;; Assignment to the function-cell require (fset 'symbol (lambda ...))
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html#Function-Cells
  ;;
  (defun smartchr-unset-before-mc ()
    "Unset particular smartchr setting conditional on major mode

This should be run before running multiple-cursors"
    (pcase major-mode
      ;; ESS
      ('ess-mode-hook               (smartchr-ess-mode-unset))
      ('inferior-ess-mode-hook      (smartchr-ess-mode-unset))
      ;; Python
      ('ein:notebook-multilang-mode (smartchr-python-mode-unset))
      ('python-mode                 (smartchr-python-mode-unset))
      ('inferior-python-mode        (smartchr-python-mode-unset))
      ;; SML
      ('sml-mode                    (smartchr-sml-mode-unset))
      ;; LaTeX
      ('LaTeX-mode                  (smartchr-LaTeX-mode-unset))
      ;; Org
      ('org-mode                    (smartchr-org-mode-unset))
      ;; Emacs lisp
      ('emacs-lisp-mode             (smartchr-emacs-lisp-mode-unset))
      ;; Haskell
      ('haskell-mode                (smartchr-haskell-mode-unset))
      ;; Ruby
      ('ruby-mode                   (smartchr-ruby-mode-unset))))
  ;;
  (defun smartchr-set-after-mc ()
    "Set particular smartchr setting conditional on major mode

This should be run after running multiple-cursors"
    (pcase major-mode
      ;; ESS
      ('ess-mode-hook               (smartchr-ess-mode-unset))
      ('inferior-ess-mode-hook      (smartchr-ess-mode-unset))
      ;; Python
      ('ein:notebook-multilang-mode (smartchr-python-mode-unset))
      ('python-mode                 (smartchr-python-mode-unset))
      ('inferior-python-mode        (smartchr-python-mode-unset))
      ;; SML
      ('sml-mode                    (smartchr-sml-mode-unset))
      ;; LaTeX
      ('LaTeX-mode                  (smartchr-LaTeX-mode-unset))
      ;; Org
      ('org-mode                    (smartchr-org-mode-unset))
      ;; Emacs lisp
      ('emacs-lisp-mode             (smartchr-emacs-lisp-mode-unset))
      ;; Haskell
      ('haskell-mode                (smartchr-haskell-mode-unset))
      ;; Ruby
      ('ruby-mode                   (smartchr-ruby-mode-unset))))
  ;;
  ;; Work around for smartchr.el
  ;; https://github.com/magnars/multiple-cursors.el/blob/master/multiple-cursors-core.el#L514-L529
  (setq multiple-cursors-mode-enabled-hook 'smartchr-unset-before-mc)
  (setq multiple-cursors-mode-disabled-hook 'smartchr-set-after-mc)
  ;;
  ;;
;;; Define major-mode specific setters and unsetters
;;;  ESS
  (defun smartchr-ess-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
    (local-set-key (kbd "+") (smartchr '("+" " + ")))
    (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------")))
    (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################")))
    (local-set-key (kbd "~") (smartchr '("~" " ~ ")))
    (local-set-key (kbd "$") (smartchr '("$" "$`!!'$")))
    (local-set-key (kbd "%") (smartchr '("%" " %`!!'% "))))
  (add-hook 'ess-mode-hook          'smartchr-ess-mode-set)
  (add-hook 'inferior-ess-mode-hook 'smartchr-ess-mode-set)
  (fset 'smartchr-ess-mode-unset (smartchr-construct-unsetter 'smartchr-ess-mode-set))
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
  (fset 'smartchr-python-mode-unset (smartchr-construct-unsetter 'smartchr-python-mode-set))
  ;;
;;;  SML
  (defun smartchr-sml-mode-set ()
    (local-set-key (kbd "=") (smartchr '(" = " " => " "=")))
    (local-set-key (kbd ":") (smartchr '(" : " "::"))))
  (add-hook 'sml-mode-hook 'smartchr-sml-mode-set)
  (fset 'smartchr-sml-mode-unset (smartchr-construct-unsetter 'smartchr-sml-mode-set))
  ;;
;;;  LaTeX
  (defun smartchr-LaTeX-mode-set ()
    (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
    (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"))))
  (add-hook 'LaTeX-mode-hook 'smartchr-LaTeX-mode-set)
  (fset 'smartchr-LaTeX-mode-unset (smartchr-construct-unsetter 'smartchr-LaTeX-mode-set))
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
  (fset 'smartchr-org-mode-unset (smartchr-construct-unsetter 'smartchr-org-mode-set))
  ;;
;;;  Emacs Lisp
  (defun smartchr-emacs-lisp-mode-set ()
    (local-set-key (kbd ";") (smartchr '("; " ";; " ";;; " ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"))))
  (add-hook 'emacs-lisp-mode-hook 'smartchr-emacs-lisp-mode-set)
  (fset 'smartchr-emacs-lisp-mode-unset (smartchr-construct-unsetter 'smartchr-emacs-lisp-mode-set))
  ;;
;;;  Haskell
  (defun smartchr-haskell-mode-set ()
    (local-set-key (kbd "=") (smartchr '(" = " " == " "=")))
    (local-set-key (kbd "+") (smartchr '(" + " " ++ " "+")))
    (local-set-key (kbd "-") (smartchr '(" - " " -> " "-")))
    (local-set-key (kbd ":") (smartchr '(" : " " :: " ":"))))
  (add-hook 'haskell-mode-hook 'smartchr-haskell-mode-set)
  (fset 'smartchr-haskell-mode-unset (smartchr-construct-unsetter 'smartchr-haskell-mode-set))
  ;;
;;;  Ruby
  (defun smartchr-ruby-mode-set ()
    (local-set-key (kbd "=") (smartchr '(" = " " == " "="))))
  (add-hook 'ruby-mode-hook 'smartchr-ruby-mode-set)
  (fset 'smartchr-ruby-mode-unset (smartchr-construct-unsetter 'smartchr-ruby-mode-set))
  ;;
  ;; Define an auto-loadable function
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
