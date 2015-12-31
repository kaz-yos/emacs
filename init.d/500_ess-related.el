;;; Emacs Speaks Statistics (ESS) for emacs
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
;; Now installed via elpa
;;
;; Some condigurations were taken from vgoulet Emacs distribution configuration files
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/
(require 'ess-site)
(require 'ess-eldoc)				; Slows cursor movements slightly?
;; (setq ess-eldoc-show-on-symbol t)		; Shows eldoc when cursor is on function name (Causes errors)
;;
;; No history, no saving!
(setq-default inferior-R-args "--no-restore-history --no-save ")
;;
;; Key assignment for delete trailing whitespace
(add-hook 'ess-mode-hook (setq ess-nuke-trailing-whitespace-p t))
(define-key ess-mode-map (kbd "s-w") 'ess-nuke-trailing-whitespace)
(global-set-key (kbd "s-w") 'ess-nuke-trailing-whitespace)
;;
;; Underscore preservation in ESS
;; http://www.r-bloggers.com/a-small-customization-of-ess/
(setq ess-S-assign-key (kbd "C-="))	; C-= gives <-
(ess-toggle-S-assign-key t)		; enable above key definition
(ess-toggle-underscore nil)		; leave my underscore key alone!
;;
;; Smart TAB completion in R scripts, similar to iESS behavior.
(setq ess-tab-complete-in-script   nil)	; Trying out nil 2013-03-03
(setq ess-first-tab-never-complete nil)	; Trying out nil 2013-03-01
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
;; ess-mode
(add-hook 'ess-mode-hook		; For ESS mode
          '(lambda()
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
;; Rnw-mode
(add-hook 'Rnw-mode-hook
          '(lambda()
	     ;;
	     ))
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
;;; ess-trace-bug.el		; filtering ++++ > ??? Not working
;; http://code.google.com/p/ess-tracebug/
;; (require 'ess-tracebug)	; Now included in ESS
(setq ess-use-tracebug t)	; permanent activation
;;
;;; Tooltip included in ESS
(setq ess-describe-at-point-method 'tooltip)		; 'tooltip or nil (buffer)
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
;; M-n s
(define-key ess-noweb-minor-mode-map (kbd "A-s") 'ess-swv-weave-PDF)
(define-key ess-noweb-minor-mode-map (kbd "H-s") 'ess-swv-weave-PDF)
(define-key poly-noweb+r-mode-map (kbd "A-s") 'ess-swv-weave-PDF)
(define-key poly-noweb+r-mode-map (kbd "H-s") 'ess-swv-weave-PDF)
;; M-n P
(define-key ess-noweb-minor-mode-map (kbd "A-p") 'ess-swv-PDF)
(define-key poly-noweb+r-mode-map (kbd "A-p") 'ess-swv-weave-PDF)
;;
;;
;;;
;;; ESS julia language
;; https://github.com/emacs-ess/ESS/wiki/Julia
;; excecutable file Changed as of 2013-12-20
(setq inferior-julia-program-name "/usr/local/bin/julia")
;;
;;
;;;
;;; ESS SAS configuration
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
(add-hook 'sas-mode-hook	; For SAS mode
          '(lambda()
             (local-unset-key [C-tab] 'ess-sas-backward-delete-tab)))	; Unset C-tab from ESS major mode
;;;

;;;
;;; ess-R-object-popup.el
;; https://github.com/myuhe/ess-R-object-popup.el
(use-package ess-R-object-popup
  :commands (ess-R-object-popup)
  :init
  ;; key config always provide
  (define-key ess-mode-map (kbd "C-c C-g") 'ess-R-object-popup)
  :config
  ;; Configuration for different objects
  (setq ess-R-object-popup-alist
        '((numeric    . "summary")
          (logical    . "summary")
          (factor     . "summary")
          (integer    . "summary")
          (lm         . "summary")
          (glm        . "summary")			; added
          (gls        . "summary")			; added
          (lme        . "summary")			; added
          (glht	    . "summary")			; added
          (survfit    . "survival:::print.survfit")	; added
          (survdiff   . "survival:::print.survdiff")	; added
          (coxph	    . "survival:::print.coxph")		; added
          (other      . "str"))))

;;
;;;
;;; Send X to ESS buffer
(defun ess-send-X ()
  "Send X to ESS buffer to resolve LaTeX error"
  (interactive)
  ;;
  (let* ((script-window (selected-window)))
    ;; Go to inferior process
    (ess-switch-to-ESS t)
    ;;
    (insert "X")
    ;; Return key
    (inferior-ess-send-input)
    ;; Come back to script
    (select-window script-window)))
;;
;; Give A-x (close to A-s for knitting)
(define-key ess-noweb-minor-mode-map (kbd "A-x") 'ess-send-X)
(define-key ess-noweb-minor-mode-map (kbd "H-x") 'ess-send-X)
(define-key poly-noweb+r-mode-map (kbd "A-x") 'ess-send-X)
(define-key poly-noweb+r-mode-map (kbd "H-x") 'ess-send-X)

;;;
;;; *.Rmd files invoke r-mode
;; Temporary fix for R markdown files.
;; poly-mode does not work well with auto-save
;; (setq auto-mode-alist
;;       (cons '("\\.Rmd$" . r-mode) auto-mode-alist))


;;;
;;; STAN support 2014-01-15
(require 'stan-mode)
;; stan-snippets.el
;; (require 'stan-snippets)
;; flymake-stan.el
;; (require 'flymake-stan)


;;;
;;; julia-mode.el
;; Official support
;; (require 'julia-mode)
