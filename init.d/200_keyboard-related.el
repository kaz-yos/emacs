;;; Keyboard modification
;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/

;;;
;;; C-h for delete (backspace)
;; https://www.emacswiki.org/emacs/BackspaceKey
(define-key key-translation-map [?\C-h] [?\C-?])
;; A-h ? will bring up what's available
;; (global-set-key (kbd "A-h") 'help-command)
;; (bind-key "A-h" 'help-command)
;; (bind-key "C-S-h" 'help-command)
(bind-key "h" 'help-command my-key-map)
;; (bind-key "C-h" 'delete-backward-char)


;;;
;;; Alt, Super, and Hyper for terminal use
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifier-Keys.html
;; C-x @ S event-apply-shift-modifier
;; C-x @ a event-apply-alt-modifier
;; C-x @ c event-apply-control-modifier
;; C-x @ h event-apply-hyper-modifier
;; C-x @ m event-apply-meta-modifier
;; C-x @ s event-apply-super-modifier
;;
;; These were taken from simple.el.gz and modified
;; (define-key function-key-map [?\C-c ?a] 'event-apply-alt-modifier)
;; (define-key function-key-map [?\C-c ?s] 'event-apply-super-modifier)
;; (define-key function-key-map [?\C-c ?h] 'event-apply-hyper-modifier)
;;
;; bind-key version
(bind-key "C-c a" 'event-apply-alt-modifier function-key-map)
(bind-key "C-c s" 'event-apply-super-modifier function-key-map)
(bind-key "C-c h" 'event-apply-hyper-modifier function-key-map)


;;;
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
  (setq mac-right-command-modifier 'super)
  ;; right option
  (setq mac-right-option-modifier 'hyper)
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

;;;
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
	w32-apps-modifier 'hyper))
