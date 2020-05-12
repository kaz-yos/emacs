;;; 500_keybinding-related.el ---                    -*- lexical-binding: t; -*-

;;;
;;; sequential-command.el
;; https://github.com/rubikitch/sequential-command
(use-package sequential-command
  :ensure t
  :commands (define-sequential-command)
  :bind (("C-a" . seq-cmd--home)
         ("C-e" . seq-cmd--end)
         ("M-u" . seq-cmd--upcase-backward-word)
         ("M-c" . seq-cmd--capitalize-backward-word)
         ("M-l" . seq-cmd--downcase-backward-word))
  :config
  (define-sequential-command seq-cmd--home
    beginning-of-line beginning-of-buffer seq-return)
  (define-sequential-command seq-cmd--end
    end-of-line end-of-buffer seq-return)
  ;;
  (defun seq-cmd--upcase-backward-word ()
    (interactive)
    (upcase-word (- (1+ (seq-count*)))))
  (defun seq-cmd--capitalize-backward-word ()
    (interactive)
    (capitalize-word (- (1+ (seq-count*)))))
  (defun seq-cmd--downcase-backward-word ()
    (interactive)
    (downcase-word (- (1+ (seq-count*))))))


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
  (use-package subr-x
    :commands (thread-first))
  ;;
;;;  Define multiple-cursors work around functions
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
;;;  Define major-mode specific setters and unsetters
  ;; fset: This function stores definition in the function cell of symbol.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html#Function-Cells
;;;   ESS
  (defun smartchr-ess-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
    (local-set-key (kbd "+") (smartchr '("+" " + ")))
    (local-set-key (kbd "-") (smartchr '("-" " - " "--------------------------------------------------------------------------------")))
    (local-set-key (kbd "#") (smartchr '("# " "## " "### " "################################################################################")))
    (local-set-key (kbd "~") (smartchr '("~" " ~ ")))
    (local-set-key (kbd "<") (smartchr '("<" " <- ")))
    (local-set-key (kbd "$") (smartchr '("$" "$`!!'$")))
    (local-set-key (kbd "%") (smartchr '("%" " %`!!'% "))))
  (add-hook 'ess-mode-hook          'smartchr-ess-mode-set)
  (add-hook 'inferior-ess-mode-hook 'smartchr-ess-mode-set)
  (fset 'smartchr-ess-mode-unset (smartchr-construct-unsetter 'smartchr-ess-mode-set))
  ;;
;;;   Python
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
;;;   SML
  (defun smartchr-sml-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " => ")))
    (local-set-key (kbd ":") (smartchr '(" : " "::"))))
  (add-hook 'sml-mode-hook 'smartchr-sml-mode-set)
  (fset 'smartchr-sml-mode-unset (smartchr-construct-unsetter 'smartchr-sml-mode-set))
  ;;
;;;   LaTeX
  (defun smartchr-LaTeX-mode-set ()
    ;; This $ one has conflict with poly-markdown.el where first $ invokes latex-mode.
    ;; (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
    ;; (local-set-key (kbd "%") (smartchr '("% " "%% " "%%% " "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")))
    )
  (add-hook 'LaTeX-mode-hook 'smartchr-LaTeX-mode-set)
  (fset 'smartchr-LaTeX-mode-unset (smartchr-construct-unsetter 'smartchr-LaTeX-mode-set))
  ;;
;;;   Org
  (defun smartchr-org-mode-set ()
    (local-set-key (kbd "^") (smartchr '("^{`!!'}" "^")))
    (local-set-key (kbd "_") (smartchr '("_{`!!'}" "_" "_`!!'_")))
    (local-set-key (kbd "/") (smartchr '("/" "/`!!'/")))
    (local-set-key (kbd "=") (smartchr '("=" "=`!!'=")))
    (local-set-key (kbd "-") (smartchr '("-" "-`!!'-")))
    (local-set-key (kbd "+") (smartchr '("+" "+`!!'+")))
    (local-set-key (kbd "$") (smartchr '("$`!!'$" "$")))
    (local-set-key (kbd "#") (smartchr '("# " "#+" "# ############################################################################ #"))))
  (add-hook 'org-mode-hook 'smartchr-org-mode-set)
  (fset 'smartchr-org-mode-unset (smartchr-construct-unsetter 'smartchr-org-mode-set))
  ;;
;;;   Emacs Lisp
  (defun smartchr-emacs-lisp-mode-set ()
    (local-set-key (kbd ";") (smartchr '("; " ";; " ";;; " ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"))))
  (add-hook 'emacs-lisp-mode-hook 'smartchr-emacs-lisp-mode-set)
  (fset 'smartchr-emacs-lisp-mode-unset (smartchr-construct-unsetter 'smartchr-emacs-lisp-mode-set))
  ;;
;;;   Haskell
  (defun smartchr-haskell-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
    (local-set-key (kbd "+") (smartchr '(" + " " ++ " "+")))
    (local-set-key (kbd "-") (smartchr '(" - " " -> " "-")))
    (local-set-key (kbd ":") (smartchr '(" : " " :: " ":"))))
  (add-hook 'haskell-mode-hook 'smartchr-haskell-mode-set)
  (fset 'smartchr-haskell-mode-unset (smartchr-construct-unsetter 'smartchr-haskell-mode-set))
  ;;
;;;   Ruby
  (defun smartchr-ruby-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == "))))
  (add-hook 'ruby-mode-hook 'smartchr-ruby-mode-set)
  (fset 'smartchr-ruby-mode-unset (smartchr-construct-unsetter 'smartchr-ruby-mode-set))
  ;;
;;;   C++
  (defun smartchr-c++-mode-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == "))))
  (add-hook 'c++-mode-hook 'smartchr-c++-mode-set)
  (fset 'smartchr-c++-mode-unset (smartchr-construct-unsetter 'smartchr-c++-mode-set))
  ;;
;;;   octave
  (defun smartchr-ocvate-set ()
    (local-set-key (kbd "=") (smartchr '("=" " = " " == "))))
  (add-hook 'ocvate-mode-hook 'smartchr-ocvate-set)
  (fset 'smartchr-ocvate-unset (smartchr-construct-unsetter 'smartchr-ocvate-set))
  ;;
  ;;
  ;; Define an auto-loadable function
  :commands (smartchr))


;;;
;;; Pop-up key navigator-related
;;;  hydra.el
;; https://github.com/abo-abo/hydra
;; https://github.com/abo-abo/hydra/wiki/Emacs
;; https://github.com/yangchenyun/emacs-prelude/blob/master/setup-hydra.el
(use-package hydra
  :ensure t
  ;; This package should be loaded without deferring
  ;; as the `eval-after-load' in the body must be set.
  :demand t
  :bind (;;
         :map my-key-map
         ("m" . hydra-multiple-cursors/body)
         ("," . hydra-expand-region/body)
         :map mode-specific-map
         ("o" . hydra-window/body)
         :map my-key-map
         ("." . hydra-highlight-symbol/body))
  :config
  ;;
  ;; Binding styles
  ;; https://github.com/abo-abo/hydra/wiki/Binding-Styles
  ;; 1. Bind heads upon defining. 2. Bind the body after defining.
  ;;
  ;; https://github.com/abo-abo/hydra/blob/master/README.md#the-rules-of-hydra-tics
  ;; (defhydra hydra-awesome (awesome-map awesome-binding awesome-plist)
  ;;   awesome-docstring
  ;;   awesome-head-1
  ;;   awesome-head-2
  ;;   awesome-head-3
  ;;   ...)
  ;;
  ;; Keyboard macro configurations
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Keyboard-Macro.html
  (defhydra hydra-kmacro (;; map key
                          global-map "<f3>"
                          ;; hydra state ends after any body
                          :exit t
                          ;; Any non-body ley terminates
                          :foreign-keys nil)
    "kmacro"
    ("3" kmacro-start-macro-or-insert-counter "Start recording")
    ("4" kmacro-end-or-call-macro "End recording or Call macro")
    ("i" kmacro-insert-counter "Insert counter")
    ("s" kmacro-start-macro "Start recording")
    ("e" kmacro-end-macro "End recording")
    ("c" kmacro-call-macro "Call macro")
    ("q" nil "Quit"))
  ;;
  ;; Minimum org-mode setting
  (eval-after-load "org"
    '(defhydra hydra-org-next-prev (org-mode-map "C-c")
       "Move to "
       ("C-n"  outline-next-visible-heading "Next visible heading")
       ("C-p"  outline-previous-visible-heading "Previous visible heading")
       ("C-f"  outline-forward-same-level "Forward same level")
       ("C-b"  outline-backward-same-level "Backward same level")
       ("C-k" nil "Cancel")
       ("k" nil "Cancel")))
  ;;
  ;; Extensive outline mode bindings
  ;; https://github.com/abo-abo/hydra/wiki/Emacs#outline-minor-mode
  (defhydra hydra-outline (;; map key
                           global-map "C-c #"
                           :color pink
                           :hint nil)
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
  (eval-after-load "org"
    '(define-key org-mode-map (kbd "C-`") 'hydra-outline/body))
  ;;
  (eval-after-load "highlight-symbol"
    (defhydra hydra-highlight-symbol (;; map key
                                      ;; Bind the body
                                      ;; my-key-map "."
                                      :exit nil
                                      ;; run keys not covered in the body.
                                      :foreign-keys run)
      "highlight-symbol"
      ("." highlight-symbol "Toggle" :column "Highlight")
      ("R" highlight-symbol-remove-all "Remove all" :column "Highlight")
      ;;
      ("n" highlight-symbol-next "Symbol" :column "Next")
      ("N" highlight-symbol-next-in-defun "In defun" :column "Next")
      ;;
      ("p" highlight-symbol-prev "Symbol" :column "Previous")
      ("P" highlight-symbol-prev-in-defun "In defun" :column "Previous")
      ;;
      ("q" nil "Quit" :column "Misc")
      ("C-g" nil "Quit" :column "Misc")))
  ;;
  (defhydra hydra-expand-region (
                                 ;; Bind the body
                                 ;; my-key-map ","
                                 :exit nil)
    "expand-region"
    ("," er/expand-region "Expand" :column "Expand")
    ("e" er/expand-region "Expand" :column "Expand")
    ("." er/contract-region "Contract" :column "Contract")
    ("c" er/contract-region "Contract" :column "Contract")
    ("q"   nil "Quit" :column "Misc")
    ("C-g" nil "Quit" :column "Misc"))
  ;;
  ;; Window related
  ;; https://github.com/abo-abo/hydra/wiki/Window-Management
  (defhydra hydra-window (;; map key
                          ;; Bind the body
                          ;; mode-specific-map "o"
                          :exit nil
                          :pre (require 'windmove))
    "windmove etc"
    ("b" windmove-left "Left" :column "Left")
    ("h" windmove-left "Left" :column "Left")
    ;;
    ("p" windmove-up "Up" :column "Vertical")
    ("k" windmove-up "Left" :column "Vertical")
    ("j" windmove-down "Left" :column "Vertical")
    ("n" windmove-down "Down" :column "Vertical")
    ;;
    ("f" windmove-right "Right" :column "Right")
    ("l" windmove-right "Right" :column "Right")
    ;;
    ("o" other-window "Other" :column "Cycle")
    ("O" previous-multiframe-window "Prev" :column "Cycle")
    ;;
    ("|" split-window-right "|New" :column "Split")
    ("/" split-window-below "/New" :column "Split")
    ("1" delete-other-windows "Only 1" :column "Split")
    ;;
    ("q" nil "Quit" :column "Misc")
    ("C-g" nil "Quit" :column "Misc"))
  ;;
  ;; https://github.com/abo-abo/hydra/wiki/multiple-cursors
  ;; Need to include the generated commands in mc/cmds-to-run-once
  ;; in the `multiple-cursors.el' configuration to avoid running
  ;; them on all current cursors.
  (eval-after-load "multiple-cursors"
    '(progn
       (defhydra hydra-multiple-cursors (;; map key
                                         my-key-map "m"
                                         :exit nil
                                         :hint nil)
         ;;
         "multiple-cursors: %(mc/num-cursors)"
         ;; Previous
         ("p"   mc/mark-previous-like-this "String" :column "Previous")
         ("<"   mc/mark-previous-like-this "String" :column "Previous")
         ("P"   mc/mark-previous-symbol-like-this "Symbol" :column "Previous")
         ("M-<" mc/mark-previous-symbol-like-this "Symbol" :column "Previous")
         ("M-p" mc/unmark-previous-like-this "Unmark" :column "Previous")
         ;; Next
         ("n"   mc/mark-next-like-this "String" :column "Next")
         (">"   mc/mark-next-like-this "String" :column "Next")
         ("N"   mc/mark-next-symbol-like-this "Symbol" :column "Next")
         ("M->" mc/mark-next-symbol-like-this "Symbol" :column "Next")
         ("M-n" mc/unmark-next-like-this "Unmark" :column "Next")
         ;; All
         ("a"   mc/mark-all-like-this "String" :column "All")
         ("*"   mc/mark-all-like-this "String" :column "All")
         ("A"   mc/mark-all-symbols-like-this "Symbol" :column "All")
         ("M-*" mc/mark-all-symbols-like-this "Symbol" :column "All")
         ;; Selected region
         ("s"   mc/mark-all-in-region-regexp "Search" :column "Region")
         ("l"   mc/edit-lines "Lines" :column "Region")
         ;; Misc
         ("0"   mc/insert-numbers "Insert numbers" :column "Misc")
         ;; Quit
         ("q"   nil "Quit" :column "Misc")
         ("C-g" nil "Quit" :column "Misc"))
       ;;
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-previous-like-this)
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-previous-symbol-like-this)
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/unmark-previous-like-this)
       ;;
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-next-like-this)
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-next-symbol-like-this)
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/unmark-next-like-this)
       ;;
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-all-like-this)
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-all-symbol-like-this)
       ;;
       (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/insert-numbers))))

;;;  major-mode-hydra.el
;; https://github.com/jerrypnz/major-mode-hydra.el
(use-package major-mode-hydra
  :ensure t
  :bind (
         :map my-key-map
         ("y" . major-mode-hydra))
  :config
  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Doc"
     (("d" helpful-at-point "thing-at-pt")
      ("f" helpful-callable "function")
      ("v" helpful-variable "variable")
      ("i" info-lookup-symbol "info lookup"))))
  )

;;;  which-key.el
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :defer 5
  :commands (which-key-show-top-level)
  ;;
  :config
  (setq which-key-lighter "")
  ;; Delay (in seconds) for which-key buffer to popup.
  (setq which-key-idle-delay 1.0)
  ;; Type: minibuffer, side-window, frame, and custom.
  (setq which-key-popup-type 'side-window)
  ;; Location
  (setq which-key-side-window-location '(left
                                         bottom))
  ;; Max size
  (setq which-key-side-window-max-height 0.5)
  (setq which-key-side-window-max-width 0.5)
  ;;
  ;; Paging
  ;; https://github.com/justbur/emacs-which-key/blob/master/README.org#method-1-default-using-c-h-or-help-char
  ;; Note C-h is by default equivalent to ? in this context.
  ;; In my configuration, C-h is DEL. Use ?-n, ?-b, etc
  (setq which-key-use-C-h-commands t)
  ;;
  (which-key-mode 1))

;;;  hercules.el
;; https://gitlab.com/jjzmajic/hercules.el
;; Unlike hydra, hercules.el entry and exit points are associated
;; with functions, not keys.
;; Calling any of the `head' via M-x invokes hercules, which stays around.
(use-package hercules
  :ensure t
  ;; Demand this package to allow for `eval-after-load'.
  :demand t
  ;;
  :config
  (eval-after-load "macrostep"
    (hercules-def :toggle-funs #'macrostep-mode
                  :keymap 'macrostep-keymap))
  ;;
  (eval-after-load "edebug"
    (hercules-def :toggle-funs #'edebug-mode
                  :keymap 'edebug-mode-map))
  ;;
  ;; 2020-05-11 Not useful unless, configuring the exit key.
  ;; (eval-after-load "debug"
  ;;   (hercules-def :toggle-funs #'debugger-mode
  ;;                 :keymap 'debugger-mode-map))
  ;;
  ;; 2020-05-09 This breaks bindings once bm is called.
  ;; (eval-after-load "bm"
  ;;   (hercules-def :toggle-funs #'bm-show-mode
  ;;                 :keymap 'bm-show-mode-map
  ;;                 :transient t))
  )


;;;
;;; bind-keys.el
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; describe-personal-keybindings can be used to check all my bindings.
(use-package bind-key
  :ensure t
  :commands (bind-key
             describe-personal-keybindings))


;;;
;;; free-keys.el
;; https://github.com/Fuco1/free-keys
;; http://emacs.stackexchange.com/questions/964/show-unbound-keys
;; Use this to see what remaining keys are available.
;; Use bind-key.el describe-personal-keybindings for used keys.
(use-package free-keys
  :ensure t
  :commands (free-keys)
  ;;
  :config
  ;; List of modifiers that can be used in front of keys.
  (setq free-keys-modifiers '(""
                              "C" "A" "M" "s" "H"
                              "C-M" "A-C" "C-s" "A-M")))


;;;
;;; iso-transl.el
;; How can I prevent/override key translation behavior such as: µ (translated from A-m) runs the command self-insert-command
;; http://emacs.stackexchange.com/questions/17508/how-can-i-prevent-override-key-translation-behavior-such-as-µ-translated-from
;; Culprit: key-translation-map (Keymap of key translations that can override keymaps)
;; The override should happen at iso-transl loading.
(with-eval-after-load 'iso-transl
  ;; Define a worker function first.
  (defun drop-Alt-letter-from-key-translation-map (letter)
    "Drop Alt-letter binding from key-translation-map to avoid translation."
    (let ((vec (vconcat letter)))
      (aset vec 0 (logior (aref vec 0) ?\A-\^@))
      ;; define-key KEYMAP KEY DEFINITION
      (define-key key-translation-map vec nil)))
  ;;
  ;; The following cancels translation of A-letter bindings.
  ;; These get translated to greek mu.
  (drop-Alt-letter-from-key-translation-map "m")
  (drop-Alt-letter-from-key-translation-map "u")
  ;; This gets translated to small o at the top.
  (drop-Alt-letter-from-key-translation-map "o"))
