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
  ;; Need to require a library defining thread-first macro?
  (require 'subr-x)
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
      ;; Commands for Binding Keys
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
      ;; (local-unset-key key) := (define-key (current-local-map) key nil)
      ;; GNU Emacs Lisp Reference Manual: Controlling Active Maps
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Controlling-Active-Maps.html
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
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-matching-case-statement.html#Pattern-matching-case-statement
    (pcase major-mode
      ;; ESS
      ('ess-mode                    (smartchr-ess-mode-unset))
      ('inferior-ess-mode           (smartchr-ess-mode-unset))
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
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-matching-case-statement.html#Pattern-matching-case-statement
    (pcase major-mode
      ;; ESS
      ('ess-mode                    (smartchr-ess-mode-set))
      ('inferior-ess-mode           (smartchr-ess-mode-set))
      ;; Python
      ('ein:notebook-multilang-mode (smartchr-python-mode-set))
      ('python-mode                 (smartchr-python-mode-set))
      ('inferior-python-mode        (smartchr-python-mode-set))
      ;; SML
      ('sml-mode                    (smartchr-sml-mode-set))
      ;; LaTeX
      ('LaTeX-mode                  (smartchr-LaTeX-mode-set))
      ;; Org
      ('org-mode                    (smartchr-org-mode-set))
      ;; Emacs lisp
      ('emacs-lisp-mode             (smartchr-emacs-lisp-mode-set))
      ;; Haskell
      ('haskell-mode                (smartchr-haskell-mode-set))
      ;; Ruby
      ('ruby-mode                   (smartchr-ruby-mode-set))))
  ;;
  ;; Work around for smartchr.el
  ;; https://github.com/magnars/multiple-cursors.el/blob/master/multiple-cursors-core.el#L514-L529
  (setq multiple-cursors-mode-enabled-hook 'smartchr-unset-before-mc)
  (setq multiple-cursors-mode-disabled-hook 'smartchr-set-after-mc)
  ;;
  ;;
;;; Define major-mode specific setters and unsetters
  ;; fset: This function stores definition in the function cell of symbol.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html#Function-Cells
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
    (local-set-key (kbd "^") (smartchr '("^" "^{`!!'}")))
    (local-set-key (kbd "_") (smartchr '("_" "_{`!!'}" "_`!!'_")))
    (local-set-key (kbd "/") (smartchr '("/" "/`!!'/")))
    (local-set-key (kbd "=") (smartchr '("=" "=`!!'=")))
    (local-set-key (kbd "-") (smartchr '("-" "-`!!'-")))
    (local-set-key (kbd "+") (smartchr '("+" "+`!!'+")))
    (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
    (local-set-key (kbd "#") (smartchr '("# " "#+" "# ############################################################################ #"))))
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
;;; smartrep.el
;; https://github.com/myuhe/smartrep.el
;; http://sheephead.homelinux.org/2011/12/19/6930/
(use-package smartrep
  :disabled t
  :config
  ;; org-mode
  (eval-after-load "org"
    '(progn
       (smartrep-define-key
        org-mode-map "C-c" '(("C-n" . (lambda ()
                                        (outline-next-visible-heading 1)))
                             ("C-p" . (lambda ()
                                        (outline-previous-visible-heading 1))))))))


;;;
;;; hydra.el
;; https://github.com/abo-abo/hydra
;; https://github.com/abo-abo/hydra/wiki/Emacs
;; https://github.com/yangchenyun/emacs-prelude/blob/master/setup-hydra.el
;; This may be outdated.
;; http://emacs.rubikitch.com/hydra/
(use-package hydra
  :commands (hydra-zoom/body)
  :config
  ;; Buffer text scaling
  ;; https://github.com/abo-abo/hydra#the-one-with-the-least-amount-of-code
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  ;;
  ;; Minimum org-mode setting
  (defhydra hydra-org-next-prev (org-mode-map "C-c")
    "Move to "
    ("C-n"  outline-next-visible-heading "Next visible heading")
    ("C-p"  outline-previous-visible-heading "Previous visible heading")
    ("C-f"  outline-forward-same-level "Forward same level")
    ("C-b"  outline-backward-same-level "Backward same level")
    ("C-k" nil "Cancel")
    ("k" nil "Cancel"))
  ;;
  ;; Extensive outline mode bindings
  ;; https://github.com/abo-abo/hydra/wiki/Emacs#outline-minor-mode
  (defhydra hydra-outline (:color pink :hint nil)
    "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
    ;; Hide
    ("q" hide-sublevels)    ; Hide everything but the top-level headings
    ("t" hide-body)         ; Hide everything but headings (all body lines)
    ("o" hide-other)        ; Hide other branches
    ("c" hide-entry)        ; Hide this entry's body
    ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all)          ; Show (expand) everything
    ("e" show-entry)        ; Show this heading's body
    ("i" show-children)     ; Show this heading's immediate child sub-headings
    ("k" show-branches)     ; Show all sub-headings under this heading
    ("s" show-subtree)      ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading)                ; Up
    ("n" outline-next-visible-heading)      ; Next
    ("p" outline-previous-visible-heading)  ; Previous
    ("f" outline-forward-same-level)        ; Forward - same level
    ("b" outline-backward-same-level)       ; Backward - same level
    ("z" nil "leave"))
  (global-set-key (kbd "C-c #") 'hydra-outline/body)
  (define-key org-mode-map (kbd "C-`") 'hydra-outline/body))


;;;
;;; bind-keys.el
;; A part of use-package.el
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; http://emacs.rubikitch.com/bind-key/
;; describe-personal-keybindings can be used to check all my bindings.
(use-package bind-key
  :commands (bind-key
             describe-personal-keybindings))


;;;
;;; guide-key.el
;; https://github.com/kai2nenobu/guide-key
;; http://www.kaichan.info/blog/2013-12-22-emacs-advent-calendar-2013-22.html
;; http://www.kaichan.info/blog/2012-12-03-emacs-advent-calendar-2012-03.html
(use-package guide-key
  :commands (guide-key-mode)
  :init
  ;; Activate on start up
  (add-hook 'after-init-hook 'guide-key-mode)
  :config
  ;; Delay in seconds before guide buffer is displayed.
  (setq guide-key/idle-delay 1.0)
  ;; Set font size (negative for smaller)
  (setq guide-key/text-scale-amount 0.1)
  ;; Show at the bottom
  ;; http://shibayu36.hatenablog.com/entry/2013/08/05/214023
  (setq guide-key/popup-window-position 'left)
  ;; Guide everything
  (setq guide-key/guide-key-sequence t)
  ;; Guide specific sequences
  ;; (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  ;; Check an input key sequence recursively.
  (setq guide-key/recursive-key-sequence-flag t))


;;;
;;; which-key.el
;; https://github.com/justbur/emacs-which-key
;; Vertical alignment breaks down by default, giving an ugly look.
(use-package which-key
  :disabled t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'left))


;;;
;;; free-keys.el
;; https://github.com/Fuco1/free-keys
;; http://emacs.stackexchange.com/questions/964/show-unbound-keys
;; Use this to see what remaining keys are available.
;; Use bind-key.el describe-personal-keybindings for used keys.
(use-package free-keys
  :commands (free-keys))
