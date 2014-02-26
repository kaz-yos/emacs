;;; Auto-completion related configurations
;; Configure ac-* in respective files. Keep this file minimum.


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
 ;; ac-candidate-limit nil
 ac-delay 0			; Faster than default 0.1 before AC kicks in
 ac-auto-show-menu 0.1		; 0.1 sec before menu appears
 ac-candidate-menu-min 1	; Show menu if 2+ candidates
 ac-menu-height 20		; 20 candidates
 ;; http://stackoverflow.com/questions/17309773/emacs-autocomplete-inside-python-string
 ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
 ac-disable-faces nil		; auto-complete everywhere, even within quotes, comments
 ;; ac-ignore-case 'smart	; Treat them smartly
 ac-ignore-case nil		; Treat them strictly
 ac-use-quick-help nil		; No pop up help!
 ;; ac-quick-help-delay 1.5
 ;; ac-quick-help-prefer-pos-tip t
 )
;; Less anoying settings
;; http://cx4a.org/software/auto-complete/manual.html#Not_to_complete_automatically
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;;
;; http://www.emacswiki.org/emacs/ESSAuto-complete
;; (define-key ac-completing-map [tab] 'ac-complete)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
;; (define-key ac-completing-map [tab] nil)
;; (define-key ac-completing-map [return] 'ac-complete)	; configured again at end
(define-key ac-completing-map (kbd "RET") 'ac-complete) ; configured again at end
;;
;; Trigger key
;; http://cx4a.org/software/auto-complete/manual.html#Trigger_Key
(ac-set-trigger-key "TAB")
;; (ac-set-trigger-key (kbd "TAB")) ; This does not work
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


;; icicles.el
;; http://www.emacswiki.org/emacs/Icicles
;; http://www.emacswiki.org/emacs/EmacsNewbieWithIcicles
(require 'icicles)
(icy-mode 1)


;; Handling of the tab completion buffer 2014-02-03
;; http://stackoverflow.com/questions/6458220/automatically-close-emacs-shell-mode-tab-completion-buffer
(defun delete-completion-window-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*")))
  output)
(add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer)
