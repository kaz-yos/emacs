;;; 500_mode-line-related.el ---                     -*- lexical-binding: t; -*-

;;;
;;; Count lines and characters in selected region
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
;; https://emacs.stackexchange.com/questions/3712/display-in-the-mode-line-the-number-of-characters-in-the-selection
(use-package emacs
  :config
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
  ;; setq-default to change the default value to avoid being overriden by subsequent changes.
  (setq-default mode-line-format
                (cons '(:eval (count-lines-and-chars)) mode-line-format)))


;;;
;;; diminish.el
;; https://github.com/myrjola/diminish.el
(use-package diminish
  :ensure t
  :config)
