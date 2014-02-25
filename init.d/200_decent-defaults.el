;;; default.el in Vincent Goulet's distribution
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/Emacs-24.3-modified-4/default.el
;; https://svn.fsg.ulaval.ca/svn-pub/vgoulet/emacs-modified/macos/tags/Emacs-23.3-modified-3/default.el
;;
;; Nice options to have On by default
(mouse-wheel-mode t)				; activate mouse scrolling
(global-font-lock-mode t)			; syntax highlighting
(transient-mark-mode t)				; sane select (mark) mode
(delete-selection-mode t)			; entry deletes marked text
;; http://ergoemacs.org/emacs/emacs_make_modern.html
;; (setq show-paren-style 'expression)		; highlight entire bracket expression (annoying)
(add-hook 'text-mode-hook 'turn-on-auto-fill)	; wrap long lines in text mode
(column-number-mode 1)				; show (row, col) number


;; Suppress all dialog boxes completely, even file open dialogue. No need for mouse!
;; http://www.gnu.org/s/libtool/manual/emacs/Dialog-Boxes.html
(setq use-dialog-box nil)


;;; Allow non-default behaviors
;; Upcase/downcase allowed
(put 'upcase-region	'disabled nil)
(put 'downcase-region	'disabled nil)


;;; ffap find-file-at-point
;; Get file path or URL from text in the current line
;; http://www.gnu.org/s/libtool/manual/emacs/FFAP.html
(ffap-bindings)


;;; Allow more variables
;; Increase max values for number of variables that can be defined 2013-09-19
;; http://mikio.github.io/article/2012/06/26_variable-binding-depth-exceeds-max-specpdl-size.html
(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 6000)


;;; CUA configurations without rectangles
;; Common User Access mode for column editing: Activated by C-RET while selecting text
;; http://tech.kayac.com/archive/emacs-rectangle.html
;; http://trey-jackson.blogspot.com/2008/10/emacs-tip-26-cua-mode-specifically.html
;; http://stackoverflow.com/questions/3750332/how-do-i-force-a-binding-in-emacs
(setq cua-rectangle-mark-key (kbd "<C-S-return>")) ; <C-S-return> for rectangle
(cua-mode t)
(setq cua-enable-cua-keys nil)			; C-x C-c C-v left intact
;; (setq cua-keep-region-after-copy t)		; Keep selection after copying (Mac/Win-like)


;;; New line at the EOF
;; Whether to add a newline automatically at the end of the file.
;; A value of t means do this only when the file is about to be saved.
(setq require-final-newline t)


;;; Faster echo in the echo area
;; Show echo like C-x fast 2014-02-20
(setq echo-keystrokes 0.1)
