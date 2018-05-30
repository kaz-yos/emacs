;;; Mode-line-related


;;;
;;; Clean mode-line when switching major modes
;; https://github.com/shibayu36/emacs/blob/8376ba4e4b4d5cb4668e848c9a494b287232ea6f/emacs.d/inits/01-mode-line.el
(defvar mode-line-cleaner-alist
  '(;; For minor-mode, first char is 'space'
    (abbrev-mode            . "")
    (anzu-mode              . "")
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
  (cl-loop for (mode . mode-str) in mode-line-cleaner-alist
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
;; setq-default to change the default value to avoid being overriden by subsequent changes.
;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
(defun count-lines-and-chars ()
  (if (and mark-active
           ;; Do not show if in mc mode.
           (not multiple-cursors-mode))
      ;; If active, show
      (let ((beg (region-beginning))
            (end (region-end)))
        (format " %dLn %dWd %dCh "
                ;; Number of lines
                (count-lines beg end)
                ;; Number of words
                (count-words beg end)
                ;; Number of characters
                (- end beg)))
    ;; If not active, empty.
    ""))
;;
(setq-default mode-line-format
              (cons '(:eval (count-lines-and-chars)) mode-line-format))
