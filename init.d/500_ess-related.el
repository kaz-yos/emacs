;;; Emacs Speaks Statistics (ESS) for emacs
;; http://ess.r-project.org
;; Now installed via elpa
(use-package ess-site
  :demand t
  :commands (R
             R-mode
             r-mode)
  ;; https://github.com/jwiegley/use-package#binding-within-local-keymaps
  :bind (:map ess-mode-map
              ("A-s" . buffer-do-async-shell-command)
              ("M-s M-s" . buffer-do-async-shell-command)
              ("C-c C-e" . ess-eval-region-or-line-and-step)
              ("C-c C-b" . ess-eval-buffer)
              ("C-c C-u" . ess-eval-buffer-from-beg-to-here)
              ("C-=" . ess-insert-assign)
              ("_" . nil)
              ;;
              :map inferior-ess-mode-map
              ("C-=" . ess-insert-assign)
              ("_" . nil))
  ;;
  ;; https://github.com/jwiegley/use-package#modes-and-interpreters
  :mode (("\\.R\\'" . r-mode)
         ("\\.sas\\'" . sas-mode)
         ("\\.jags\\'" . ess-jags-mode)
         ("\\.Rnw" . poly-noweb+r-mode)
         ("\\.Rmd" . poly-markdown+r-mode))
  :interpreter (("R" . r-mode)
                ("SAS" . sas-mode))
  ;;
  ;;
  :init
  ;; The org-mode exporter for Julia requires this variable.
  (setq inferior-julia-program-name (executable-find "julia"))
  ;;
  ;;
  :config
  (message "ess config being run!!")
  ;;
;;;  poly-R.el
  (use-package poly-R
    :commands (poly-noweb+r-mode
               poly-markdown+r-mode)
    ;; These mode association is overwritten by ess-site
    ;; So include the same thing in ess-site :mode
    :bind (:map polymode-mode-map
                ("C-c n" . polymode-next-chunk-same-type)
                ("C-c p" . polymode-previous-chunk-same-type)
                ("A-n" . polymode-next-chunk-same-type)
                ("A-p" . polymode-previous-chunk-same-type)
                ("A-s" . polymode-export)
                ("M-s M-s" . polymode-export))
    ;; Config for poly-R.
    :config
    ;; Auto revert for .Rmd
    (add-hook 'poly-markdown+r-mode-hook 'turn-on-auto-revert-mode)
    ;;
    ;; For some reason beginning-of-line appears in the post-self-insert-hook while editing latex in Rmd.
    (defun remove-beginning-of-line-from-post-self-insert-hook ()
      "Remove beginning-of-line from post-self-insert-hook."
      (interactive)
      (remove-hook 'post-self-insert-hook 'beginning-of-line))
    (defun clear-post-self-insert-hook ()
      "Wipe out post-self-insert-hook."
      (interactive)
      (setq post-self-insert-hook '()))
    ;;
    ;; Execute all R chunks at once from an Rmd document
    ;; https://polymode.github.io/usage/#evaluation-of-chunks
    ;; https://stackoverflow.com/questions/40894202/execute-all-r-chunks-at-once-from-an-rmd-document
    ;; https://github.com/polymode/poly-R/issues/3
    ;;
    ;; Naming chunks
    (defun rmd-name-chunks ()
      "Name chunks in current RMarkdown file."
      (interactive)
      (let ((buffer-file-extension (file-name-extension (buffer-file-name))))
        (when (string= buffer-file-extension "Rmd")
          (shell-command (concat "Rscript"
                                 " -e "
                                 "'"
                                 "namer::name_chunks("
                                 "\""
                                 buffer-file-name
                                 "\""
                                 ")'")))))
    (defun rmd-unname-all-chunks ()
      "Unname all chunks in current RMarkdown file."
      (interactive)
      (let ((buffer-file-extension (file-name-extension (buffer-file-name))))
        (when (string= buffer-file-extension "Rmd")
          (shell-command (concat "Rscript"
                                 " -e "
                                 "'"
                                 "namer::unname_all_chunks("
                                 "\""
                                 buffer-file-name
                                 "\""
                                 ")'"))))))
  ;;
;;;  Remaining config
  (defun my-ess-bring-up-inferior-ess ()
    (interactive)
    ;; Move to iESS
    (ess-switch-to-inferior-or-script-buffer nil)
    ;; Move back
    (ess-switch-to-inferior-or-script-buffer nil))
  ;;
  ;; If non-nil activate flymake in ess-mode buffers.
  (setq ess-use-flymake nil)
  ;; No history, no saving!
  (setq-default inferior-R-args "--no-restore-history --no-save ")
  ;; If t ess will try to use ido completion whenever possible.
  (setq ess-use-ido nil)
  ;;
  ;; Bind key at the time of loading.
  ;; Then this calls smart-jump-go/back autoloads elsewhere.
  (bind-key "M-." 'smart-jump-go ess-mode-map)
  (bind-key "M-," 'smart-jump-back ess-mode-map)
  ;;
  ;; Smart TAB completion in R scripts, similar to iESS behavior.
  (setq ess-tab-complete-in-script nil)
  (setq ess-first-tab-never-complete nil)
  ;;
  ;; Must-haves for ESS
  ;; http://www.emacswiki.org/emacs/CategoryESS
  (setq ess-eval-visibly 'nowait)		; New in 12.09-1
  (setq ess-ask-for-ess-directory nil)	; Don't ask for directory
  ;;
  ;; Auto-scrolling of R console to bottom and Shift key extension
  ;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
  ;; Adapted with one minor change from Felipe Salazar at
  ;; http://www.emacswiki.org/emacs/ESSShiftEnter
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  ;;
;;;  Package development
  ;; Do not work in the package environment automatically.
  ;; Can toggle with M-x ess-r-set-evaluation-env.
  ;; http://emacs.1067599.n8.nabble.com/turning-off-ess-r-package-mode-td406874.html
  (setq ess-r-package-auto-set-evaluation-env nil)
  ;;
  (setq ess-roxy-template-alist
        '(("description" .  ".. content for \\description{} (no empty lines) ..")
          ("details" . ".. content for \\details{} ..")
          ("" . "")
          ("param" .  "")
          ("return" . "")
          ("author" . ess-user-full-name)))
  ;;
  ;; R buffer name manipulation
  ;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
  ;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
  ;; (setq ess-gen-proc-buffer-name-function (lambda (proc) (concatenate 'proc "R")))
  ;; (setq ess-gen-proc-buffer-name-function (lambda (proc) "nameR"))
  ;;
  ;; Function to toggle $ in syntax table 2013-08-06
  (defun toggle-dollar ()
    "Toggle status of $ and @ in the syntax table"
    (interactive)
    (if (equal " " (char-to-string (char-syntax ?$)))
        (progn	; Change to symbol
          (modify-syntax-entry ?$  "_"  S-syntax-table)
          (modify-syntax-entry ?@  "_"  S-syntax-table))
      (progn	; Change to white space (space between symbols)
        (modify-syntax-entry ?$  " "  S-syntax-table)
        (modify-syntax-entry ?@  " "  S-syntax-table))
      ))
  ;;
  ;; R-mode
  (add-hook 'R-mode-hook
            '(lambda()
               ;; Toggling $ in S-syntax-table
               (local-set-key (kbd "C-c 4") 'toggle-dollar)	; Toggle $ in S-syntax-table
               (modify-syntax-entry ?$  " "  S-syntax-table)	; $ as whitespace in S
               ;; Additional keybinds
               (local-set-key (kbd "C-c f") 'ess-eval-function)
               (local-set-key (kbd "S-<return>") 'ess-eval-region-or-function-or-paragraph-and-step)
               ))
  ;;
  ;; inferior-ess-mode
  (add-hook 'inferior-ess-mode-hook	; For iESS mode
            '(lambda()
               (local-set-key (kbd "C-c w") 'ess-execute-screen-options)	; To adjust width
               ;; (local-set-key [C-up] 'comint-previous-input)
               ;; (local-set-key [C-down] 'comint-next-input)
               (local-set-key (kbd "C-<up>") 'comint-previous-input)
               (local-set-key (kbd "C-<down>") 'comint-next-input)
               (local-set-key (kbd "C-c 4") 'toggle-dollar)	; Toggle $ in S-syntax-table
               ;; (modify-syntax-entry ?$  "_"  S-syntax-table)	; $ as symbol in iESS. not working
               ))
  ;;
  ;; https://stat.ethz.ch/pipermail/ess-help/2009-July/005455.html
  (add-hook 'ess-post-run-hook
            '(lambda ()
               ;; Extend column width of R-Process in Emacs
               (ess-execute-screen-options)))			; Reset screen width
  ;;
  ;; Rd-mode-hook
  (add-hook 'Rd-mode-hook
            '(lambda()
               ;; Toggling $ in S-syntax-table
               (local-set-key (kbd "C-c 4") 'toggle-dollar)	; Toggle $ in S-syntax-table
               (modify-syntax-entry ?$  " "  S-syntax-table)	; $ as whitespace in S
               ))
  ;;
  ;;
;;; ess-trace-bug.el
  (setq ess-use-tracebug t)	; permanent activation
  ;;
;;; Tooltip included in ESS
  (setq ess-describe-at-point-method 'tooltip)		; 'tooltip or nil (buffer)
  ;;
;;; setwd() from Emacs
  (defun my-ess-setwd ()
    "setwd() to the current R script's parent directory"
    (interactive)
    (let ((dir-name (abbreviate-file-name (file-name-directory (buffer-file-name))))
          (r-process (get-process ess-local-process-name)))
      (ess-send-string r-process (concat "setwd('" dir-name "')") t)))
  ;;
  ;;
;;; Reproducible research with knitr, etc
  ;; Use knitr for .Rnw document
  (setq ess-swv-processor 'knitr)
  ;; Add commands to AUCTeX's M-x TeX-command-list
  (setq ess-swv-plug-into-AUCTeX-p t)
  ;; Supress ess-swv-PDF from opening PDF by meaning less value
  ;; http://tex.stackexchange.com/questions/69660/sweave-how-to-suppress-opening-new-instance-of-a-pdf-when-running-pdflatex
  (setq ess-pdf-viewer-pref "ls")
  ;;
  ;; Define a function to flush ess shell
  ;; http://stackoverflow.com/questions/3447531/emacs-ess-version-of-clear-console
  (defun ess-flush-shell ()
    "Flush ESS shell"
    (interactive)
    (let* (;; Assign the current buffer
           (script-window (selected-window))
           (proc (get-process ess-local-process-name)))
      ;; Change ESS shell (goes to bottom)
      (ess-switch-to-ESS t)
      ;; Flush
      (comint-truncate-buffer)
      ;; Come back to the script
      (select-window script-window)
      ;; Return nil (this is a void function)
      nil))
  ;;
  ;; Define a one step function for .Rnw 2013-09-10
  (defun ess-swv-weave-PDF ()
    "One step function to sweave and create PDF"
    (interactive)
    ;; nil to run with default processor without asking
    (ess-swv-weave nil)
    ;;
    ;; Instead of (ess-swv-PDF), do the same from R console
    (let* ((Rnw-buffer-file-name (buffer-file-name))
           (dir-name (file-name-directory Rnw-buffer-file-name))
           (namestem (file-name-sans-extension Rnw-buffer-file-name))
           (latex-filename (concat namestem ".tex"))
           (r-process (get-process ess-local-process-name)))
      ;; defun ess-send-string (process string &optional visibly message)
      ;; setwd() to current .Rnw's directory (otherwise PDF goes to R's current dir
      (ess-send-string r-process (concat "setwd('" dir-name "')") t)
      ;; Invoke texi2pdf on the .tex file
      ;; file_name.Rnw.pdf is generated intentionally
      (ess-send-string r-process (concat "system('texi2pdf "
                                         latex-filename
                                         " -o "
                                         Rnw-buffer-file-name
                                         ".pdf"
                                         "')") t)
      ;; Flush shell to prevent heavy log from staying around
      (ess-flush-shell)))
  ;;
;;;
;;; ESS SAS configuration
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
  (add-hook 'sas-mode-hook
            '(lambda()
               ;; Unset C-tab from ESS major mode in SAS mode
               (local-unset-key [C-tab] 'ess-sas-backward-delete-tab)))
;;;
;;; ess-R-object-popup.el
  ;; https://github.com/myuhe/ess-R-object-popup.el
  (use-package ess-R-object-popup
    :commands (ess-R-object-popup)
    :bind (:map ess-mode-map
                ("C-c C-g" . ess-R-object-popup))
    :config
    ;; Configuration for different objects
    (setq ess-R-object-popup-alist
          '((numeric    . "summary")
            (logical    . "summary")
            (factor     . "summary")
            (integer    . "summary")
            (lm         . "summary")
            ;; Added
            (glm        . "summary")
            (gls        . "summary")
            (lme        . "summary")
            (glht       . "summary")
            (survfit    . "survival:::print.survfit")
            (survdiff   . "survival:::print.survdiff")
            (coxph      . "survival:::print.coxph")
            ;;
            (other      . "str"))))
  )


;;;
;;; stan-mode.el
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :commands (my-stan-setup
             company-stan-backend)
  :hook (stan-mode . my-stan-setup)
  ;;
  :config
  ;; These variables are not directly used.
  (setq stan-comment-start "//")
  (setq stan-comment-end "")
  ;; Rather, they have to be set to buffer-local comment-start/end variables.
  (defun my-stan-setup ()
    (setq comment-start stan-comment-start)
    (setq comment-end stan-comment-end)
    ;; Two-character indent.
    ;; https://mc-stan.org/docs/2_18/stan-users-guide/white-space.html
    (setq c-basic-offset 2)
    ;; Add company-stan-backend to the company-backends buffer-locally.
    (add-to-list (make-local-variable 'company-backends)
                 'company-stan-backend))
  ;;
  ;; Writing backends for the company-mode.
  ;; https://github.com/company-mode/company-mode/wiki/Writing-backends
  ;; http://sixty-north.com/blog/series/how-to-write-company-mode-backends.html
  ;; Definitions
  ;; https://github.com/company-mode/company-mode/blob/master/company.el
  ;; Example in company-math.el
  ;; https://github.com/vspinu/company-math/blob/master/company-math.el#L210
  ;;
  ;; stan-keywords-lists.el defines the following:
  ;;  stan-types-list
  ;;  stan-function-return-types-list
  ;;  stan-blocks-list
  ;;  stan-range-constraints-list
  ;;  stan-keywords-list
  ;;  stan-functions-list
  ;;  stan-distribution-list
  ;;  stan-reserved-list
  ;;  Do NOT USE stan-deprecated-function-list
  ;;
  (defun propertize-list (lst category)
    "Propertize each element of a string list"
    (mapcar (lambda (s)
              (propertize s :category category))
            lst))
  ;;
  (defun company-stan-backend-annotation (s)
    "Construct an annotation string from the :category property"
    (format " [%s]" (get-text-property 0 :category s)))
  ;;
  ;; The signature (command &optional arg &rest ignored) is mandated.
  (defun company-stan-backend (command &optional arg &rest ignored)
    "company-mode backend function for stan-mode"
    ;; Making it interactive allows interactive testing.
    (interactive (list 'interactive))
    ;; (cl-case EXPR (KEYLIST BODY...)...)
    ;; Eval EXPR and choose among clauses on that value.
    ;; Here we decide what to do based on COMMAND.
    ;; One of {interactive, prefix, candidates, annotation}
    (cl-case command
      ;; 1. interactive call
      ;; (company-begin-backend BACKEND &optional CALLBACK)
      ;; Start a completion at point using BACKEND.
      (interactive (company-begin-backend 'company-stan-backend))
      ;; 2. prefix command
      ;;  It should return the text that is to be completed.
      ;;  If it returns nil, this backend is not used.
      ;;  Here we need to verify the major mode.
      (prefix (and (eq major-mode 'stan-mode)
                   ;; If point is at the end of a symbol, return it for completion.
                   ;; Otherwise, if point is not inside a symbol, return an empty string.
                   ;; This will give the prefix to be completed.
                   (company-grab-symbol)))
      ;; 3. candidates command
      ;;  This is where we actually generate a list of possible completions.
      ;;  When this is called arg holds the prefix string to be completed
      (candidates
       (cl-remove-if-not
        ;; Retain if matching
        (lambda (c) (my-company-fuzzy-match arg c))
        ;; from a long list of all stan object names.
        (append (propertize-list stan-types-list "type")
                (propertize-list stan-function-return-types-list "function return type")
                (propertize-list stan-blocks-list "block")
                (propertize-list stan-range-constraints-list "range constraint")
                (propertize-list stan-keywords-list "keyword")
                (propertize-list stan-functions-list "function")
                (propertize-list stan-distribution-list "distribution")
                (propertize-list stan-reserved-list "reserved")
                )))
      ;; 4. annotation command
      (annotation (company-stan-backend-annotation arg))))
  )
