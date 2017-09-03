;;; Mode-line-related


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
    (ivy-mode               . "")
    (magit-auto-revert-mode . "")
    (paredit-mode           . "")
    (super-save-mode        . "")
    (undo-tree-mode         . "")
    (yas-minor-mode         . "")
    (elisp-slime-nav-mode   . "")
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
;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)
;; (remove-hook 'after-change-major-mode-hook 'clean-mode-line)


;;;
;;; Count lines and characters in selected region
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
;; https://emacs.stackexchange.com/questions/3712/display-in-the-mode-line-the-number-of-characters-in-the-selection
(defun count-lines-and-chars ()
  (if mark-active
      ;; If active, show
      (format "%d lines, %d words, %d chars "
              ;; Number of lines
              (count-lines (region-beginning) (region-end))
              ;; Number of words
              (count-words (region-beginning) (region-end))
              ;; Number of characters
              (- (region-end) (region-beginning)))
    ;; If not active, empty.
    ""))
;;
(add-to-list 'mode-line-format
             '(:eval (count-lines-and-chars)))
