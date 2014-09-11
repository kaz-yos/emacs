;;; Auto-completion related configurations
;; Configure ac-* in respective files. Keep this file minimum.

;;;
;;; hippie-expand (built-in)
;; hippie-expand instead of less functional dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)


;;;
;;; AUTO-COMPLETE
;;; auto-complete.el, auto-complete-config.el, fuzzy.el, popup.el downloaded from below URL
;; https://github.com/auto-complete/auto-complete
;; http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete)
;; Activate ac everywhere
(global-auto-complete-mode t)
;;
;; Activate ac in predefined modes
(require 'auto-complete-config)
(ac-config-default)
;;
;; Auto-complete for ESS configuration
;; http://www.emacswiki.org/emacs/ESSAuto-complete
(setq
 ;; Limit number of candidates
 ;; ac-candidate-limit nil
 ;;
 ;; Number of letters before ac kicks in
 ac-auto-start 1
 ;;
 ;; Delay to completions will be available.
 ac-delay 0.1
 ;;
 ;; 0.1 sec before menu appears
 ac-auto-show-menu 0.2
 ;;
 ;; Show menu if 2+ candidates
 ac-candidate-menu-min 1
 ;;
 ;; 20 candidates at a time
 ac-menu-height 20
 ;;
 ;; Where to disable ac
 ;; http://stackoverflow.com/questions/17309773/emacs-autocomplete-inside-python-string
 ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
 ;; auto-complete everywhere, even within quotes, comments
 ;; ac-disable-faces nil
 ac-disable-faces '(font-lock-comment-face)
 ;;
 ;; Treat cases strictly by nil or smartly by 'smart
 ac-ignore-case nil
 ;;
 ;; Pop up help
 ac-use-quick-help t
 ac-quick-help-delay 1.0
 ac-quick-help-prefer-pos-tip t
 )
;; Key configuration
;; Non-nil means a special keymap `ac-menu-map' on completing menu will be used.
;; http://cx4a.org/software/auto-complete/manual.html#Not_to_complete_automatically
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n")    'ac-next)
(define-key ac-menu-map (kbd "C-p")    'ac-previous)
(define-key ac-menu-map (kbd "<tab>")  'ac-next)
(define-key ac-menu-map (kbd "<S-tab>")'ac-previous)
;;
;; http://www.emacswiki.org/emacs/ESSAuto-complete
(define-key ac-completing-map (kbd "<tab>")    'ac-complete)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)
;;
;; Trigger key
;; http://cx4a.org/software/auto-complete/manual.html#Trigger_Key
(ac-set-trigger-key "TAB")
;;
;; If you are using 'flyspell' you might want to activate the workaround
;; http://www.emacswiki.org/emacs/AutoComplete#toc6
(ac-flyspell-workaround)
;;
;; popup.el
;; https://github.com/auto-complete/popup-el (called automatically)
;; (require 'popup)
;; Prevent broken popup
;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
(setq popup-use-optimized-column-computation nil)


;;;
;;; icicles.el		; Minibuffer input completion and cycling.
;; http://www.emacswiki.org/emacs/Icicles
;; Nutshell
;; http://www.emacswiki.org/emacs/Icicles_-_Nutshell_View
;; Newbie
;; http://www.emacswiki.org/emacs/EmacsNewbieWithIcicles
;; Customization and General Tips
;; http://www.emacswiki.org/emacs/Icicles_-_Customization_and_General_Tips
;; Key Bindings
;; http://www.emacswiki.org/emacs/Icicles_-_Key_Bindings
;; Modal cycling
;; http://www.emacswiki.org/emacs/Icicles_-_Customization_and_General_Tips#icicle-modal-cycle-up-keys
;;
;; Need to turn off icicle-mode, and turn it back on to reflect configuration changes.
(require 'icicles)
(icy-mode 1)
;;
;; Initial height decrease for text in buffer `*Completions*'. (0.75 by default)
;; http://www.emacswiki.org/emacs/Icicles_-_Customization_and_General_Tips#icicle-Completions-text-scale-decrease
(setq icicle-Completions-text-scale-decrease 0.0)
;;
;;; Default cycling mode to be used before you hit ‘TAB’ or ‘S-TAB’.
;; prefix or apropos (fuzzy matching)
(setq icicle-default-cycling-mode 'apropos)
;;
;;; Key configuration for modal cycling within minibuffer
;; No need for configuration. They already work if configured for apropos-cycle.
;; (add-to-list 'icicle-modal-cycle-up-keys   (kbd "C-p"))
;; (add-to-list 'icicle-modal-cycle-down-keys (kbd "C-n"))
;;
;;; Key configuration for cycling fuzzy matching
;; icicle-apropos-complete-keys: S-tab by default
(setq icicle-apropos-complete-keys (list (kbd "<tab>")))
;; icicle-apropos-cycle-previous/next-keys: [next]/[prior] by default
(setq icicle-apropos-cycle-previous-keys (list (kbd "<A-tab>") (kbd "C-p") (kbd "<prior>")))
(setq icicle-apropos-cycle-next-keys     (list                 (kbd "C-n") (kbd "<next>")))
;;
;;; Key configuration for cycling prefix matching
;; icicle-prefix-complete-keys: tab by default
(setq icicle-prefix-complete-keys (list (kbd "<S-tab>")))
;; icicle-prefix-cycle-previous/next-keys: [home]/[end] by default
(setq icicle-prefix-cycle-previous-keys (list (kbd "<S-A-tab>") (kbd "<home>")))
(setq icicle-prefix-cycle-next-keys     (list                   (kbd "<end>")))
;;



;; ;;; ido.el and flx-ido.el		; flx integration for ido
;; ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; ;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; ;; http://miyazakikenji.wordpress.com/2013/06/11/emacs-に-ido-mode/
;; ;;
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)


;;;
;;; grizzl.el		; A fuzzy-search utility for Emacs
;; https://github.com/d11wtq/grizzl


;;;
;;; Handling of the tab completion buffer 2014-02-03
;; http://stackoverflow.com/questions/6458220/automatically-close-emacs-shell-mode-tab-completion-buffer
(defun delete-completion-window-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*")))
  output)
(add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer)



;;; popwin.el		; Popup Window Manager
;; https://github.com/m2ym/popwin-el
;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
(require 'popwin)
;; (popwin-mode 1)
;; Where to show
(setq popwin:popup-window-position 'left)
;; width 20% of frame width
(setq popwin:popup-window-width 0.2)
;; height 30% of frame height
(setq popwin:popup-windowheightwidth 0.3)
;;
;; Buffers under control (default minus help-mode)
(setq popwin:special-display-config
      '(;; ("*Completions*")
	(completion-list-mode :noselect t)
	;; (compilation-mode :noselect t)
	(grep-mode :noselect t)
	(occur-mode :noselect t)
	("*Pp Macroexpand Output*" :noselect t)
	("*Shell Command Output*")
	("*vc-diff*")
	("*vc-change-log*")
	(" *undo-tree*" :width 60 :position right)
	("^\\*anything.*\\*$" :regexp t)
	("*slime-apropos*")
	("*slime-macroexpansion*")
	("*slime-description*")
	("*slime-compilation*" :noselect t)
	("*slime-xref*")
	(sldb-mode :stick t)
	(slime-repl-mode)
	(slime-connection-list-mode)))
;;
;; Add buffers under control
;; http://aikotobaha.blogspot.com/2013/04/popwinel.html
;; Completions
;; (push '("*Completions*") popwin:special-display-config)

