;;; Keyboard modification
;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/

;;; C-h for delete (backspace)
;; http://www.emacswiki.org/emacs-en/BackspaceKey
(define-key key-translation-map [?\C-h] [?\C-?])
;; C-x ? for help instead
(define-key global-map (kbd "C-x ?") 'help-command)


;;; C-m for newline-and-indent
(global-set-key (kbd "C-m") 'newline-and-indent)


;;; Mac-only configuration to use command and options keys
(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Command key as Meta key, Option key untouched
  ;; http://www.emacswiki.org/emacs/MetaKeyProblems#toc15
  ;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
  ;;
  ;; left command
  (setq mac-command-modifier 'meta)
  ;; left option
  (setq mac-option-modifier 'alt)
  ;;
  ;; right command
  (setq mac-right-command-modifier 'hyper)
  ;; right option
  (setq mac-right-option-modifier 'super)
  ;;
  ;; Mac Binding modifier keys
  ;; http://www.emacswiki.org/emacs/EmacsForMacOS#toc23
  ;; mac-function-modifier
  ;; mac-control-modifier
  ;; mac-command-modifier
  ;; mac-option-modifier
  ;; mac-right-command-modifier
  ;; mac-right-control-modifier
  ;; mac-right-option-modifier
  ;; values can be 'control (C), 'alt (A), 'meta (M), 'super (s), or 'hyper (H).
  ;; setting to nil allows the OS to assign values
  )


;;; Windows only configuration
;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
(when (eq system-type 'w32)
  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
  (setq w32-pass-lwindow-to-system nil
	w32-pass-rwindow-to-system nil
	w32-pass-apps-to-system nil
	;; Left Windows key
	w32-lwindow-modifier 'super
	;; Right Windows key
	w32-rwindow-modifier 'super
	;; Menu key
	w32-apps-modifier 'hyper)
  )
