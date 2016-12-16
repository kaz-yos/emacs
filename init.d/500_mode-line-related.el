;;; Mode-line-related


;;;
;;; powerline.el
;; https://github.com/milkypostman/powerline
;; http://shibayu36.hatenablog.com/entry/2014/02/11/160945
(use-package powerline
  :config
  ;; https://github.com/shibayu36/emacs/blob/8376ba4e4b4d5cb4668e848c9a494b287232ea6f/emacs.d/inits/01-mode-line.el
  (set-face-attribute 'mode-line nil
                      :foreground "black"
                      :background "gray90"
                      :box nil)
  (set-face-attribute 'powerline-active1 nil
                      :foreground "black"
                      :background "gray80"
                      :inherit 'mode-line)
  (set-face-attribute 'powerline-active2 nil
                      :foreground "black"
                      :background "gray70"
                      :inherit 'mode-line)
  ;; Configure at the end
  (powerline-default-theme))


;;;
;;; spaceline.el
;; Powerline theme from Spacemacs
;; https://github.com/TheBB/spaceline
(use-package spaceline
  :disabled t
  (spaceline-spacemacs-theme))


;;;
;;; Clean mode-line when switching major modes
;; https://github.com/shibayu36/emacs/blob/8376ba4e4b4d5cb4668e848c9a494b287232ea6f/emacs.d/inits/01-mode-line.el
(defvar mode-line-cleaner-alist
  '(;; For minor-mode, first char is 'space'
    (abbrev-mode            . "")
    (anzu-mode              . "")
    (company-mode           . "")
    (eldoc-mode             . "")
    (elisp-slime-nav-mode   . "")
    (flymake-mode           . "")
    (git-gutter-mode        . "")
    (guide-key-mode         . "")
    (icicle-mode            . "")
    (ivy-mode               . "")
    (magit-auto-revert-mode . "")
    (paredit-mode           . "")
    (super-save-mode        . "")
    (undo-tree-mode         . "")
    (yas-minor-mode         . "")
    ;;
    ;; Major modes
    (dired-mode             . "Dir")
    (emacs-lisp-mode        . "elisp")))
;;
(defun clean-mode-line ()
  "Clean mode-line expressions"
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))
;;
(add-hook 'after-change-major-mode-hook 'clean-mode-line)
