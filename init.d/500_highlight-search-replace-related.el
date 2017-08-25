;;;
;;; crosshairs.el
;; http://www.emacswiki.org/emacs/CrosshairHighlighting
(use-package crosshairs
  :commands (crosshairs-mode))


;;;
;;; highligh-symbol.el
;; http://nschum.de/src/emacs/highlight-symbol/
;; http://stackoverflow.com/questions/385661/emacs-highlight-all-occurences-of-a-word
(use-package highlight-symbol
  :commands (highlight-symbol-at-point
             highlight-symbol-remove-all
             my-highlight-symbol-next
             my-highlight-symbol-prev)
  :bind (("C-." . highlight-symbol-at-point)
         ("C-\}" . my-highlight-symbol-next)
         ("C-\{" . my-highlight-symbol-prev)
         ("A-]" . my-highlight-symbol-next)
         ("A-[" . my-highlight-symbol-prev)
         ("A-M-n" . my-highlight-symbol-next)
         ("A-M-p" . my-highlight-symbol-prev)
         ("A-M-]" . my-highlight-symbol-next)
         ("A-M-[" . my-highlight-symbol-prev))
  ;;
  :config
  (setq highlight-symbol-idle-delay 0)
  ;;
  ;; Define highlight-symbol-prev/next and recenter
  (defun my-highlight-symbol-prev ()
    (interactive)
    (highlight-symbol-prev)
    (recenter))
  (defun my-highlight-symbol-next ()
    (interactive)
    (highlight-symbol-next)
    (recenter)))


;;;
;;; anzu.el
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
;; http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(use-package anzu
  :demand
  :config
  (setq anzu-mode-lighter "")
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 1)
  (global-anzu-mode +1))


;;;
;;; multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el
;; http://ongaeshi.hatenablog.com/entry/20121205/1354672102 (for a similar package)
;; http://emacsrocks.com/e13.html (video)
;; http://rubikitch.com/2014/11/10/multiple-cursors/
(use-package multiple-cursors
  :demand ; otherwise mode-line override does not work
  :init
  ;; Where to save command list
  (setq mc/list-file (concat user-emacs-directory
                             "mc-lists.el"))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ;; highlighting symbols only
         ("C-M->" . mc/mark-next-symbol-like-this)
         ("C-M-<" . mc/mark-previous-symbol-like-this)
         ("C-M-*" . mc/mark-all-symbols-like-this)
         ;; highlighting all
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this))
  :config
  ;; What to display in the mode line while multiple-cursors-mode is active.
  (setq mc/mode-line
        ;; This requires anzu.el
        `(" mc:" (:eval (format ,(propertize "%d" 'face 'anzu-mode-line)
                                (mc/num-cursors))))))


;;;
;;; SWIPER-RELATED
;;;  swiper.el
;; https://github.com/abo-abo/swiper
;; http://pragmaticemacs.com/emacs/dont-search-swipe/
;; http://pragmaticemacs.com/emacs/search-or-swipe-for-the-current-word/
(use-package swiper
  :commands (swiper
             swiper-at-point)
  :bind (("s-s" . swiper-at-point)
         ("C-s-s" . swiper)
         ;; Add bindings to isearch-mode
         :map isearch-mode-map
         ("s-s" . swiper-from-isearch))
  ;;
  :config
  (defun swiper-at-point ()
    "Custom function to pick up a thing at a point for swiper

If a selected region exists, it will be searched for by swiper
If there is a symbol at the current point, its textual representation is
searched. If there is no symbol, empty search box is started."
    (interactive)
    (swiper (selection-or-thing-at-point))))

;;;  swiper-helm.el
;; https://github.com/abo-abo/swiper-helm
(use-package swiper-helm
  :commands (swiper-helm
             swiper-helm-from-isearch)
  ;; :bind (("s-s" . swiper-helm-at-point)
  ;;        ("C-s-s" . swiper-helm)
  ;;        :map isearch-mode-map
  ;;        ("s-s" . swiper-helm-from-isearch))
  :config
  (defun swiper-helm-at-point ()
    "Custom function to pick up a thing at a point for swiper-helm

If a selected region exists, it will be searched for by swiper-helm
If there is a symbol at the current point, its textual representation is
searched. If there is no symbol, empty search box is started."
    (interactive)
    (swiper-helm (selection-or-thing-at-point))))


;;;
;;; isearch the selected word 2014-02-01
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode
           mark-active
           (not (eq (mark) (point))))
      ;; If true
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ;; If false
    ad-do-it))


;;;
;;; MULTIPLE FILE GREP AND EDIT RELATED
;;;  color-moccur.el/moccur-edit.el
;; Requires color-moccur.el (elpa)
;; http://www.bookshelf.jp/elc/moccur-edit.el
;; http://d.hatena.ne.jp/higepon/20061226/1167098839
;; http://d.hatena.ne.jp/sandai/20120304/p2
(use-package color-moccur
  :commands (moccur-grep
             occur-by-moccur
             moccur-grep-find)
  :bind (("s-m" . moccur-grep)
         ("C-s-m" . occur-by-moccur)
         ;; https://github.com/jwiegley/use-package#the-basics
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  ;;
  :config
  (setq isearch-lazy-highlight t)
  (setq moccur-following-mode-toggle t)
  ;; Editing moccur buffer
  ;; Usage:
  ;; M-x moccur-grep to enter Moccur-grep, then objectName .R$
  ;; r/C-x C-q/C-c C-i to enter Moccur-edit. C-x C-s to save, C-c C-k
  (use-package moccur-edit
    :config
    ;; Modified buffers are saved automatically.
    (defadvice moccur-edit-change-file
        (after save-after-moccur-edit-buffer activate)
      (save-buffer))))


;;;  wgrep.el
;; https://github.com/mhayashi1120/Emacs-wgrep
;; C-c C-e : Apply the changes to file buffers.
;; C-c C-u : All changes are unmarked and ignored.
;; C-c C-d : Mark as delete to current line (including newline).
;; C-c C-r : Remove the changes in the region (these changes are not
;;           applied to the files. Of course, the remaining
;;           changes can still be applied to the files.)
;; C-c C-p : Toggle read-only area.
;; C-c C-k : Discard all changes and exit.
;; C-x C-q : Exit wgrep mode.
;;
;; 27.4 Searching with Grep under Emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Grep-Searching.html
(use-package wgrep
  :commands (wgrep-setup)
  :init
  (add-hook 'grep-setup-hook 'wgrep-setup)
  :config
  ;; r/C-x C-q/C-c C-i to enter edit mode. C-x C-s to save, C-c C-k
  (define-key grep-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "C-c C-i") 'wgrep-change-to-wgrep-mode))


;;;  ag.el
;; https://github.com/Wilfred/ag.el
;; http://agel.readthedocs.io/en/latest/index.html
;; https://github.com/ggreer/the_silver_searcher
;; http://yukihr.github.io/blog/2013/12/18/emacs-ag-wgrep-for-code-grep-search/
;; ag.el
(use-package ag
  :if (executable-find "ag")
  :commands (ag
             ag-dired
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp)
  :bind (("s-a" . ag)
         ("C-s-a" . ag-project))
  ;;
  :config
  ;; grouping is better.
  (setq ag-arguments '("--smart-case" "--stats" "--group"))
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t)
  ;;
  ;; Switch to *ag search* after ag.
  ;; http://kotatu.org/blog/2013/12/18/emacs-ag-wgrep-for-code-grep-search/
  (defun my/filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
  (defun my/get-buffer-window-list-regexp (regexp)
    "Return list of windows whose buffer name matches regexp."
    (my/filter #'(lambda (window)
                   (string-match regexp
                                 (buffer-name (window-buffer window))))
               (window-list)))
  (defun switch-to-ag (&optional _1 _2 _3)
    "Switch to a buffer named *ag ... Arguments are ignored."
    (select-window ; select ag buffer
     (car (my/get-buffer-window-list-regexp "^\\*ag "))))
  ;; Advice all user functions.
  (loop for f in '(ag
                   ag-files
                   ag-regexp
                   ag-project
                   ag-project-files
                   ag-project-regexp)
        do (advice-add f :after #'switch-to-ag))
  ;;
;;;   wgrep-ag.el
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  (use-package wgrep-ag
    :bind (:map ag-mode-map
                ("C-x C-q" . wgrep-change-to-wgrep-mode))
    :config
    ;; To save buffer automatically when `wgrep-finish-edit'.
    (setq wgrep-auto-save-buffer t)
    ;; To apply all changes wheather or not buffer is read-only.
    (setq wgrep-change-readonly-file t)
    ;;
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))


;;;  rg.el
;; https://github.com/dajva/rg.el
;; https://github.com/BurntSushi/ripgrep
;; https://github.com/BurntSushi/ripgrep#installation (binary is called rg)
;; $ brew install ripgrep
(use-package rg
  :if (executable-find "rg")
  :commands (rg)
  ;;
  :config
  ;; List of command line flags for rg.
  (setq rg-command-line-flags '())
  ;; Group matches in the same file together.
  (setq rg-group-result t)
  ;; wgrep compatibility (requires wgrep-ag.el)
  (add-hook 'rg-mode-hook 'wgrep-ag-setup))


;;;
;;; highlight-sexp.el
;; http://www.emacswiki.org/emacs/HighlightSexp
;; Color M-x list-colors-display  to check good colors
(use-package highlight-sexp
  :commands (highlight-sexp-mode)
  :config
  ;; (setq hl-sexp-background-color "thistle1")
  ;; (setq hl-sexp-background-color "snow1")
  ;; (setq hl-sexp-background-color "CadetBlue1") ; for light background
  (setq hl-sexp-background-color "dark red") ; for dark background
  ;; (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
  ;; (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
  ;; (add-hook 'ess-mode-hook 'highlight-sexp-mode)	; Not turned on by default use sx to toggle
  (global-set-key (kbd "s-x") 'highlight-sexp-mode))


;;;
;;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region)))


;;;
;;; embrace.el
;; Add/Change/Delete pairs based on `expand-region', similar to `evil-surround'.
;; https://github.com/cute-jumper/embrace.el
(use-package embrace
  :commands (embrace-commander))


;;;
;;; cmigemo (installed from Homebrew)
;; Used brew to install cmigemo
;; Used M-x list-package to install migemo.el (github)
;; Configured refering to: http://d.hatena.ne.jp/ground256/20111008/1318063872
;; Works by advising isearch-mode
;;
;; Mac-only configuration
(use-package migemo
  :if (executable-find "cmigemo")
  :commands (migemo-init)
  ;;
  :init
  (add-hook 'after-init-hook 'migemo-init)
  ;;
  :config
  (setq migemo-command (executable-find "cmigemo"))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-regex-dictionary nil)
  ;; Advising
  (migemo-init))


;;;
;;; selected.el
;; https://github.com/Kungsgeten/selected.el
(use-package selected
  :commands selected-minor-mode
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)))


;;;
;;; rainbow-mode.el
;; Make strings describing colors appear in colors
;; http://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :commands (rainbow-mode))


;;;
;;; Temprary fix for void cua-replace-region
;; https://github.com/Fuco1/smartparens/issues/271
(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))


;;;
;;; AVY-RELATED
;;;  avy.el
;; More powerful reimplementation of ace-jump-mode
;; https://github.com/abo-abo/avy
;; http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/
(use-package avy
  :demand
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1
             avy-goto-word-or-subword-1
             avy-isearch)
  :bind (("s-l" . avy-goto-line)
         ("C-M-s" . avy-goto-char-timer)
         :map isearch-mode-map
         ("s-a" . avy-isearch)
         ("C-'" . avy-isearch))
  :config
  ;; Darken background.
  (setq avy-background t)
  ;; Highlight the first decision char with `avy-lead-face-0'.
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-highlight-first
  (setq avy-highlight-first t)
  ;; The default method of displaying the overlays.
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
  (setq avy-style 'at-full)
  ;; Keys to be used. Use a-z.
  (setq avy-keys (loop for c from ?a to ?z collect c))
  ;;
  ;; Time out for *-timer functions
  (setq avy-timeout-seconds 0.3)
  ;;
  ;; avy version of one-step activation
  ;; https://github.com/cjohansen/.emacs.d/commit/65efe88
  (defun add-keys-to-avy (prefix c &optional mode)
    (define-key global-map
      (read-kbd-macro (concat prefix (string c)))
      `(lambda ()
         (interactive)
         (funcall (cond
                   ;; Word beginning
                   ((eq ',mode 'word)  #'avy-goto-word-1)
                   ;; Anywhere
                   (t                  #'avy-goto-char))
                  ,c))))
  ;;
  ;; Assing key bindings for all characters
  (loop for c from ?! to ?~ do (add-keys-to-avy "M-s-" c))
  (loop for c from ?! to ?~ do (add-keys-to-avy "H-M-" c))
  (loop for c from ?! to ?~ do (add-keys-to-avy "C-M-s-" c 'word)))

;;;  avy-migemo.el
;; https://github.com/momomo5717/avy-migemo
;; http://dev.classmethod.jp/tool/emacs-avy-migemo/
(use-package avy-migemo
  :commands (avy-migemo-goto-char
             avy-migemo-goto-char-2
             avy-migemo-goto-char-in-line
             avy-migemo-goto-char-timer
             avy-migemo-goto-subword-1
             avy-migemo-goto-word-1
             avy-migemo-isearch
             avy-migemo--overlay-at
             avy-migemo--overlay-at-full
             avy-migemo--read-candidates))

;;;  ace-window.el
;; Window selection using avy.el (no dependency on ace-jump-mode.el)
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :commands (ace-select-window
             ace-swap-window)
  :bind ("s-5" . ace-window))

;;;  ace-jump-helm-line.el
;; Depends on avy.el not ace-jump-mode.el
;; https://github.com/cute-jumper/ace-jump-helm-line
(use-package ace-jump-helm-line
  :bind (:map helm-map
              ("s-l" . ace-jump-helm-line)
              ("C-'" . ace-jump-helm-line)))
