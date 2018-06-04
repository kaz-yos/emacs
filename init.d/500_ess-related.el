;;; Emacs Speaks Statistics (ESS) for emacs
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
;; Now installed via elpa
;;
;; Some condigurations were taken from vgoulet Emacs distribution configuration files
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/
(use-package ess-site
  :commands (R
             R-mode
             r-mode)
  :bind (:map ess-mode-map
              ("A-s" . buffer-do-async-shell-command)
              ("M-s M-s" . buffer-do-async-shell-command)
              ("C-c C-e" . ess-eval-region-or-line-and-step)
              ("C-c C-b" . ess-eval-buffer)
              ("C-c C-u" . ess-eval-buffer-from-beg-to-here)
              ;;
              :map ess-noweb-minor-mode-map
              ("A-s" . ess-swv-weave-PDF)
              ("A-p" . ess-swv-PDF)
              ;;
              )
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
                ("M-s M-s" . polymode-export)
                ;;
                ;; This map no longer exists?
                ;; :map poly-noweb+r-mode-map
                ;; ("A-s" . ess-swv-weave-PDF)
                ;; ("A-p" . ess-swv-PDF)
                )
    ;;
;;;  Remaining config
    ;; If non-nil activate flymake in ess-mode buffers.
    (setq ess-use-flymake nil)
    ;; Auto revert for .Rmd
    (add-hook 'poly-markdown+r-mode-hook 'turn-on-auto-revert-mode)
    ;;
    ;; Execute all R chunks at once from an Rmd document
    ;; https://stackoverflow.com/questions/40894202/execute-all-r-chunks-at-once-from-an-rmd-document
    (defun rmd-send-chunk ()
      "Send current R chunk to ess process."
      (interactive)
      (and (eq (oref pm/chunkmode :mode) 'r-mode)
           (pm-with-narrowed-to-span
            nil
            (goto-char (point-min))
            (forward-line)
            (ess-eval-region (point) (point-max) nil nil 'R))))
    ;;
    (defun rmd-send-chunks-above ()
      "Send all R code chunks above point."
      (interactive)
      (save-restriction
        (widen)
        (save-excursion
          (pm-map-over-spans
           'rmd-send-chunk (point-min) (point))))))
  ;;
  ;; No history, no saving!
  (setq-default inferior-R-args "--no-restore-history --no-save ")
  ;;
  ;; If t ess will try to use ido completion whenever possible.
  (setq ess-use-ido nil)
  ;;
  ;; Bind key at the time of loading.
  ;; Then this calls smart-jump-go/back autoloads elsewhere.
  (bind-key "M-." 'smart-jump-go ess-mode-map)
  (bind-key "M-," 'smart-jump-back ess-mode-map)
  ;;
  ;; Underscore preservation in ESS
  ;; http://www.r-bloggers.com/a-small-customization-of-ess/
  ;; C-= gives <-
  (setq ess-S-assign-key (kbd "C-="))
  ;; enable above key definition
  (ess-toggle-S-assign-key t)
  ;; leave my underscore key alone!
  (ess-toggle-underscore nil)
  (define-key ess-mode-map (kbd "_") nil)
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
  :mode ("\\.stan\\'" . stan-mode))

;;;
;;; julia-mode.el
;; Official support
;; (require 'julia-mode)
