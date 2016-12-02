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
(use-package auto-complete
  :commands (auto-complete-mode)
  :init
  ;; Configure default sources
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-filename
                             ac-source-words-in-same-mode-buffers))
  ;;
  :config
  ;; Auto-complete for ESS configuration
  ;; http://www.emacswiki.org/emacs/ESSAuto-complete
  (setq
   ;; Limit number of candidates
   ;; ac-candidate-limit nil
   ;;
   ;; Number of letters before ac kicks in
   ac-auto-start 1
   ;; Delay to completions will be available.
   ac-delay 0.1
   ;; 0.1 sec before menu appears
   ac-auto-show-menu 0.2
   ;; Show menu if 2+ candidates
   ac-candidate-menu-min 1
   ;; 20 candidates at a time
   ac-menu-height 20
   ;; Where to disable ac
   ;; http://stackoverflow.com/questions/17309773/emacs-autocomplete-inside-python-string
   ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
   ;; auto-complete everywhere, even within quotes, comments
   ;; ac-disable-faces nil
   ac-disable-faces '(font-lock-comment-face)
   ;; Treat cases strictly by nil or smartly by 'smart
   ac-ignore-case nil
   ;;
   ;; Pop up help
   ac-use-quick-help t
   ac-quick-help-delay 1.0
   ac-quick-help-prefer-pos-tip t)
  ;;
  ;; Key configuration
  ;; Non-nil means a special keymap `ac-menu-map' on completing menu will be used.
  ;; http://cx4a.org/software/auto-complete/manual.html#Not_to_complete_automatically
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n")     'ac-next)
  (define-key ac-menu-map (kbd "C-p")     'ac-previous)
  (define-key ac-menu-map (kbd "<tab>")   'ac-next)
  (define-key ac-menu-map (kbd "<S-tab>") 'ac-previous)
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
  ;; Prevent broken popup
  ;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
  (setq popup-use-optimized-column-computation nil))


;;;
;;; COMPANY-MODE (COMPlete ANYthing)
;;; company.el
;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io
;; http://www.emacswiki.org/CompanyMode
;; http://qiita.com/syohex/items/8d21d7422f14e9b53b17
;; http://qiita.com/sune2/items/b73037f9e85962f5afb7
;; http://emacs.stackexchange.com/questions/3654/filename-completion-using-company-mode
;; http://blog.binchen.org/posts/emacs-auto-completion-for-non-programmers.html
;; http://comments.gmane.org/gmane.emacs.ess.general/9037
;;
(use-package company
  :config
  ;; company-mode everywhere
  (add-hook 'after-init-hook 'global-company-mode)
  ;; except in these modes
  ;; (setq company-global-modes '(not eshell-mode ielm-mode))
  ;;
  ;; Delay time
  (setq company-idle-delay 1)
  ;; 2 letters before completion kicks in
  (setq company-minimum-prefix-length 2)
  ;; Wrap after the final candidate
  (setq company-selection-wrap-around t)
  ;; Now donwcasing
  (setq company-dabbrev-downcase nil)
  ;; Keys
  (global-set-key (kbd "A-<tab>") 'company-complete)
  (global-set-key (kbd "A-i") 'company-complete)
  ;; Selection
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; Narrowing
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  ;;
  ;; Backends (Adding globally is also ok; backends are context-aware)
  ;; http://emacs.stackexchange.com/questions/17537/best-company-backends-lists
  ;; Emacs lisp (one of default backends; somehow not included originally)
  ;; (add-to-list 'company-backends 'company-elisp) ; adding globally
  ;; Add locally
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook '(lambda ()
                      (add-to-list (make-local-variable 'company-backends)
                                   'company-elisp)))))
;;
;; Color configuration (done in init-customize.el)
;; http://www.emacswiki.org/CompanyMode#toc6
;; auto-complete-like color setting
;; https://github.com/tungd/dotfiles/blob/9af85f57fa0a31e7edd0b9c8c8ddf6a2061b6550/emacs/themes/custom-theme.el#L36-L46
;;
;;; company-statistics.el
;; Sort completion candidates by previous completion choices
;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :commands (company-statistics-mode))
(add-hook 'company-mode-hook 'company-statistics-mode)
;;
;;
;;; company-quickhelp.el
;; Documentation popup for Company
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :disabled t
  :config
  (company-quickhelp-mode 1))
;;
;;; company-try-hard.el
;; https://github.com/Wilfred/company-try-hard
(use-package company-try-hard
  :commands (company-try-hard)
  :bind
  ("A-C-<tab>" . company-try-hard))


;;;
;;; icicles.el
;; Enhances minibuffer completion
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
;;
;; Less annoying for C-x C-f than ido. I only use find-file to create a file or open a directory.
(use-package icicles
  :disabled t
  :demand
  :config
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
  (setq icicle-apropos-complete-keys       (list (kbd "<tab>")))
  ;; icicle-apropos-cycle-previous/next-keys: [next]/[prior] by default
  (setq icicle-apropos-cycle-previous-keys (list (kbd "C-p") (kbd "<prior>")))
  (setq icicle-apropos-cycle-next-keys     (list (kbd "C-n") (kbd "<next>")))
  ;;
;;; Key configuration for cycling prefix matching
  ;; icicle-prefix-complete-keys: tab by default
  (setq icicle-prefix-complete-keys (list (kbd "<S-tab>")))
  ;; icicle-prefix-cycle-previous/next-keys: [home]/[end] by default
  (setq icicle-prefix-cycle-previous-keys (list (kbd "<home>")))
  (setq icicle-prefix-cycle-next-keys     (list (kbd "<end>")))
  ;;
  ;; Drop unnecessary bindings
  ;; http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
  (define-key icicle-mode-map (kbd "C-c '") nil)
  (add-hook 'icicle-mode-hook #'(lambda ()
                                  (local-unset-key (kbd "C-c '"))
                                  ;; Trying to call *-map in here breaks config
                                  ;; (define-key icicle-mode-map (kbd "C-c '") nil)
                                  ;; (define-key org-mode-map (kbd "C-c '") 'org-edit-src-code)
                                  ))
  )



;;;
;;; IDO-RELATED
;;
;;; ido.el
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; http://miyazakikenji.wordpress.com/2013/06/11/emacs-に-ido-mode/
(use-package ido
  :demand
  :config
  ;; Flexible matching
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1)
  ;; https://www.masteringemacs.org/article/introduction-to-ido-mode
  (setq ido-use-filename-at-point 'guess)
  ;;
  ;;
;;; ido-ubiquitous.el
  ;; https://github.com/DarwinAwardWinner/ido-ubiquitous
  (require 'ido-ubiquitous)
  ;; ido for all completing-read
  (ido-ubiquitous-mode 1)
  ;;
  ;;
;;; flx-ido.el
  (require 'flx-ido)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil)
  ;;
  ;;
;;; ido-vertical-mode.el
  ;; http://rubikitch.com/2015/01/06/ido-vertical-mode/
  (require 'ido-vertical-mode)
  ;; height
  (setq ido-max-window-height 0.75)
  (ido-vertical-mode 1))



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
;; http://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening
(add-hook 'minibuffer-exit-hook 'delete-completion-window-buffer)



;;; popwin.el
;; https://github.com/m2ym/popwin-el
;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
(use-package popwin
  :disabled t
  :config
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
  )
