;;; 500_window-management-packages.el ---            -*- lexical-binding: t; -*-

;;;
;;; sticky-windows.el
(use-package sticky-windows
  :commands (sticky-window-keep-window-visible
             sticky-window-delete-window
             sticky-window-delete-other-windows))


;;;
;;; windresize for M-x windresize
;; M-x windresize, arrows, C-g for cancel, RET to save
(use-package windresize
  :commands windresize)


;;;
;;; window-number.el for direct movement to windows
(use-package window-number
  :config
  ;; No need to show numbers in the mode-line.
  ;; (window-number-mode 1)
  ;;
  (defun my-window-number-select (number)
    "Selects the nth window."
    (interactive "P")
    (if (integerp number)
        (let ((window (nth (1- number) (window-number-list))))
          (if (and window (or (not (window-minibuffer-p window))
                              (minibuffer-window-active-p window)))
              (select-window window)
            ;; if not found, activate ace-window
            (ace-select-window)))))
  ;;
  (global-set-key (kbd "s-1") (lambda () (interactive) (my-window-number-select 1)))
  (global-set-key (kbd "s-2") (lambda () (interactive) (my-window-number-select 2)))
  (global-set-key (kbd "s-3") (lambda () (interactive) (my-window-number-select 3)))
  (global-set-key (kbd "s-4") (lambda () (interactive) (my-window-number-select 4))))


;;;
;;; zoom-window.el
;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :commands (zoom-window-zoom)
  :config
  (setq zoom-window-use-elscreen t)
  (zoom-window-setup))


;;;
;;; window-purpose.el
;; https://github.com/bmag/emacs-purpose
(use-package window-purpose
  :commands (purpose-mode)
  :config
  ;; Purpose Configuration:
  ;; Customize `purpose-user-mode-purposes', `purpose-user-name-purposes',
  ;; `purpose-user-regexp-purposes' and
  ;; `purpose-use-default-configuration'.
  ;;
  ;; Basic Usage:
  ;; 1. Load/Save window/frame layout (see `purpose-load-window-layout',
  ;;    `purpose-save-window-layout', etc.)
  ;; 2. Use regular switch-buffer functions - they will not mess your
  ;;    window layout (Purpose overrides them).
  ;; 3. If you don't want a window's purpose/buffer to change, dedicate
  ;;    the window:
  ;;    C-c , d: `purpose-toggle-window-purpose-dedicated'
  ;;    C-c , D: `purpose-toggle-window-buffer-dedicated'
  ;; 4. To use a switch-buffer function that ignores Purpose, prefix it
  ;;    with C-u. For example, [C-u C-x b] calls
  ;;    `switch-buffer-without-purpose'.
  ;;
  ;; Activate
  (purpose-mode -1))
