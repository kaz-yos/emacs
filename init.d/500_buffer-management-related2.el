;;; Buffer-releated configurations with external dependencies  -*- lexical-binding: t; -*-


;;;
;;; Swap buffers with C-x /
;; http://stackoverflow.com/questions/1510091/with-emacs-how-do-you-swap-the-position-of-2-windows
(defun swap-buffer ()
  "Swap buffers between current and other window"
  (let* ((buffer-a (current-buffer))
         ;; Get second window
         (window-b (cadr (window-list)))
         ;; GEt buffer in second window
         (buffer-b (window-buffer window-b)))
    ;; Swapping
    (set-window-buffer window-b buffer-a)
    (switch-to-buffer buffer-b)
    (other-window 1)))
;;
(defun swap-buffer-plus ()
  "Swap buffers among windows

When there is only one window, split it into two.
When there are two windows, swap the buffers.
When there are three or more windows, call ace-swap-window.
Dependency: ace-swap-window"
  (interactive)
  (pcase (count-windows)
    (`1 (display-buffer (other-buffer)))
    (`2 (swap-buffer))
    (_  (ace-swap-window))))
;;
(global-set-key (kbd "C-x /") 'swap-buffer-plus)


;;;
;;; reveal-in-finder.el
;; https://github.com/kaz-yos/elisp
(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  ;; Add path to developmental repo
  ;; (when (file-exists-p "~/Documents/programming/emacs-lisp-repos/reveal-in-osx-finder")
  ;;   (add-to-list 'load-path "~/Documents/programming/emacs-lisp-repos/reveal-in-osx-finder"))
  :commands (reveal-in-osx-finder)
  :bind (:map my-key-map
              ("r" . reveal-in-osx-finder)))
