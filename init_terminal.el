;;; Minimum emacs configuration for quick launch in terminal.

;;; Configure alias in .bashrc
;; alias emacs="emacs -nw -q -l ~/.emacs.d/init_terminal.el"

;;; C-h for delete (backspace)
;; http://www.emacswiki.org/emacs-en/BackspaceKey
(define-key key-translation-map [?\C-h] [?\C-?])
;; C-x ? for help instead
(define-key global-map (kbd "C-x ?") 'help-command)
