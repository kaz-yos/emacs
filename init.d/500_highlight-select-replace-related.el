;;; crosshairs.el: highlight current line/column using hl-line(+).el/col-highlight.el
;; http://www.emacswiki.org/emacs/CrosshairHighlighting
(require 'crosshairs)
;; (toggle-crosshairs-when-idle t) ; No need for crosshairs when idle
;; (col-highlight-set-interval 60)
;;
;; hl-line+.el: highlight current line only (no column)
;; http://www.emacswiki.org/emacs/HighlightCurrentLine#toc3
;; (require 'hl-line+)		; required by crosshairs already
;; (toggle-hl-line-when-idle t)	; turned on line highlight when idle
;; (toggle-hl-line-when-idle nil)	; turned off line highlight when idle
(hl-line-when-idle-interval 60)
;;
;; To customize the background color
;; (setq my-highlight-color "light goldenrod yellow")
(setq my-highlight-color "dark red")
(set-face-background 'hl-line	    my-highlight-color) ; Line color
(set-face-background 'col-highlight my-highlight-color)	; Column color
;; (set-face-background 'hl-line	    "light goldenrod yellow")   ; Line color
;; (set-face-background 'col-highlight "light goldenrod yellow")	; Column color
;; (set-face-background 'hl-line "lemon chiffon")		; Line color
;; (set-face-background 'col-highlight "lemon chiffon")		; Column color


;;; auto-highlight-symbol.el for highlighting multiple occurences
;; https://github.com/emacsmirror/auto-highlight-symbol
(require 'auto-highlight-symbol)			; Not good with highlight-symbol.el
;; (global-auto-highlight-symbol-mode t)


;;; highligh-symbol for highlighting multiple occurences
;; http://nschum.de/src/emacs/highlight-symbol/
;; http://stackoverflow.com/questions/385661/emacs-highlight-all-occurences-of-a-word
(require 'highlight-symbol)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-\}") 'highlight-symbol-next)
;; (global-set-key (kbd "C-\{") 'highlight-symbol-prev)
;;
;; Define highlight-symbol-prev/next and recenter
(defun my-highlight-symbol-prev ()
  (interactive)
  (highlight-symbol-prev)
  (recenter))
(defun my-highlight-symbol-next ()
  (interactive)
  (highlight-symbol-next)
  (recenter))
;;
(global-set-key (kbd "C-\}") 'my-highlight-symbol-next)
(global-set-key (kbd "C-\{") 'my-highlight-symbol-prev)


;;; anzu.el 2014-02-01
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
;; http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(require 'anzu)
;;
(global-anzu-mode +1)
(setq anzu-mode-lighter "")
(setq anzu-use-migemo t)
(setq anzu-search-threshold 1000)
(setq anzu-minimum-input-length 3)
;;
;; Define a large face (also used for multiple-cursors.el)
;; This was done in custom-set-faces.
;;
;; (global-set-key (kbd "C-c r") 'anzu-query-replace)
;; (global-set-key (kbd "C-c R") 'anzu-query-replace-regexp)


;;; multiple-cursors for simultaneous editing multiple occurrences
;; https://github.com/magnars/multiple-cursors.el
;; http://ongaeshi.hatenablog.com/entry/20121205/1354672102 (for a similar package)
;; http://emacsrocks.com/e13.html (video)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
;;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;
;; What to display in the mode line while multiple-cursors-mode is active.
(setq mc/mode-line
      ;; `(" mc:" (:eval (format ,(propertize "%d" 'face 'font-lock-warning-face)
      ;; `(" mc:" (:eval (format ,(propertize "%d" 'face 'anzu-mode-line)	; Requires anzu.el
      `(" mc:" (:eval (format ,(propertize "%d" 'face 'anzu-mode-line)	; Requires anzu.el
			      (mc/num-cursors)))))


;;; isearch the selected word 2014-02-01
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))


;;; moccur-edit.el (el-get)
;; Requires color-moccur.el (elpa)
;; http://www.bookshelf.jp/elc/moccur-edit.el
;; http://d.hatena.ne.jp/higepon/20061226/1167098839
;; http://d.hatena.ne.jp/sandai/20120304/p2
(require 'moccur-edit)
;; Modified buffers are saved automatically.
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))
;; Usage:
;; M-x moccur-grep-find to enter Moccur-grep, then objectName .R$
;; r to enter Moccur-edit. C-x C-s to save, C-c C-k


;;; ag.el and wgrep-ag.el. Faster replacement for moccur-edit.el 2014-01-14
;; http://yukihr.github.io/blog/2013/12/18/emacs-ag-wgrep-for-code-grep-search/
;; https://github.com/Wilfred/ag.el
;; ag.el
(require 'ag)
(setq ag-arguments '("--smart-case" "--group" "--column" "--"))	; grouping is better.
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(setq ag-reuse-window t)
;;
;; wgrep-ag.el
(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
;; r in ag result buffer invokes edit mode. C-x C-s to save. C-x C-k to cancel.
(define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)


;;; highlight-sexp.el
;; http://www.emacswiki.org/emacs/HighlightSexp
;; Color M-x list-colors-display  to check good colors
(require 'highlight-sexp)
;; (setq hl-sexp-background-color "thistle1")
;; (setq hl-sexp-background-color "snow1")
;; (setq hl-sexp-background-color "CadetBlue1") ; for light background
(setq hl-sexp-background-color "dark red") ; for dark background
;; (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
;; (add-hook 'ess-mode-hook 'highlight-sexp-mode)	; Not turned on by default use sx to toggle


;;; expand-region.el
;; https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)
