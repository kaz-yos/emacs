;;; Keyboard modification

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
  ;;
  ;; Remove default both options to meta binding
  (setq mac-option-modifier nil)
  ;; right option as meta
  (setq mac-right-option-modifier 'meta)
  ;; left option as super
  (setq mac-left-option-modifier 'super)
  ;; command as meta
  (setq mac-command-modifier 'meta)
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
  ;; values can be 'control, 'alt, 'meta, 'super, 'hyper, nil (setting to nil allows the OS to assign values)
  )
