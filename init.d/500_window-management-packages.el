;;; 500_window-management-packages.el ---            -*- lexical-binding: t; -*-


;;;
;;; windresize.el
;; M-x windresize, arrows, C-g for cancel, RET to save
(use-package windresize
  :ensure t
  :bind-keymap (("C-c m w" . windresize-map))
  :commands windresize)


;;;
;;; window-number.el
;; https://github.com/nikolas/window-number/
(use-package window-number
  :ensure t
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
;;; shackle.el
;; https://depp.brause.cc/shackle/
(use-package shackle
  :ensure t
  :config
  ;; When t, select every window that is already displaying the buffer
  ;; otherwise only do that if the :select keyword is present.
  (setq shackle-select-reused-windows nil)
  ;; Default alignment of aligned windows.
  (setq shackle-default-alignment 'below)
  ;; Default size of aligned windows.
  (setq shackle-default-size 0.4)
  ;;
  ;; Association list of rules what to do with windows.
  ;; Example configurations
  ;; https://mullikine.github.io/posts/making-shackle-split-sensibly/
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el
  (setq shackle-rules
        '((magit-status-mode
           :select t
           :inhibit-window-quit t
           :same t)
          ;; The runner buffer also has this major mode.
          ;; (shell-mode
          ;;  :select t
          ;;  :inhibit-window-quit t
          ;;  :same t)
          ))
  ;;
  ;; Activate
  (shackle-mode 1))
