;;;
;;; highligh-symbol.el
;; http://nschum.de/src/emacs/highlight-symbol/
;; http://stackoverflow.com/questions/385661/emacs-highlight-all-occurences-of-a-word
(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol
             highlight-symbol-remove-all
             my-highlight-symbol-next
             my-highlight-symbol-prev)
  :bind (("C-." . highlight-symbol)
         ("A-]" . my-highlight-symbol-next)
         ("A-[" . my-highlight-symbol-prev)
         ("A-M-n" . my-highlight-symbol-next)
         ("A-M-p" . my-highlight-symbol-prev)
         ("A-M-]" . my-highlight-symbol-next)
         ("A-M-[" . my-highlight-symbol-prev)
         :map my-key-map
         ("." . highlight-symbol))
  ;;
  :config
  (setq highlight-symbol-idle-delay 1.5)
  ;; (highlight-symbol-mode 1)
  ;;
  (setq highlight-symbol-highlight-single-occurrence t)
  (setq highlight-symbol-colors
        '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
          "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab"))
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
  :ensure t
  :defer 2
  :config
  (setq anzu-mode-lighter "")
  (setq anzu-use-migemo (executable-find "cmigemo"))
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
  :ensure t
  ;; Need to load at start up. Otherwise, mode-line override does not work.
  :demand t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ;; highlighting symbols only
         ("C-M->" . mc/mark-next-symbol-like-this)
         ("C-M-<" . mc/mark-previous-symbol-like-this)
         ("C-M-*" . mc/mark-all-symbols-like-this)
         ;; highlighting all
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)
         ;;
         ;; These are defined in hydra.el
         ;; :map my-key-map
         ;; (">" . mc/mark-next-like-this)
         ;; ("<" . mc/mark-previous-like-this)
         ;; ("*" . mc/mark-all-like-this)
         )
  :config
  ;; whitelisting/blacklisting file
  (setq mc/list-file (concat user-emacs-directory
                             "mc-lists"
                             "_"
                             (system-name-sans-domain)
                             ".el"))
  ;; This force mc to think the `mc/list-file' is loaded.
  (setq mc--list-file-loaded t)
  ;; Black list functions explicitly here rather than in the file.
  ;; All so add elements via `add-to-list' in other places.
  (setq mc/cmds-to-run-once '())
  ;; Disables whitelisting and always executes commands for every fake cursor.
  ;; `mc/cmds-to-run-once' blacklisting is still respected.
  (setq mc/always-run-for-all t)
  ;;
  ;; What to display in the mode line while multiple-cursors-mode is active.
  ;; Do not show anything at the minor mode part.
  (setq mc/mode-line nil)
  ;; (setq mc/mode-line
  ;;       ;; This requires anzu.el
  ;;       `(" mc:" (:eval (format ,(propertize "%d" 'face 'anzu-mode-line)
  ;;                               (mc/num-cursors)))))
  (defun mode-line-mc-num-cursors ()
    ;; Non-nil if Multiple-Cursors mode is enabled.
    (if multiple-cursors-mode
        ;; If active, show
        `("mc:" ,(format (propertize "%d" 'face 'anzu-mode-line)
                         (mc/num-cursors)))
      ;; If not active, empty.
      ""))
  ;;
  (setq-default mode-line-format
                (cons '(:eval (mode-line-mc-num-cursors))
                      mode-line-format)))


;;;
;;; SWIPER-RELATED
;;;  swiper.el
;; https://github.com/abo-abo/swiper
;; http://pragmaticemacs.com/emacs/dont-search-swipe/
;; http://pragmaticemacs.com/emacs/search-or-swipe-for-the-current-word/
(use-package swiper
  :ensure t
  :commands (swiper
             swiper-at-point)
  :bind (("s-s" . swiper-at-point)
         ;; Add bindings to isearch-mode
         :map isearch-mode-map
         ("s-s" . swiper-from-isearch)
         ("C-c C-s" . swiper-from-isearch))
  ;;
  :config
  (defun swiper-at-point (u-arg)
    "Custom function to pick up a thing at a point for swiper

If a selected region exists, it will be searched for by swiper
If there is a symbol at the current point, its textual representation is
searched. If there is no symbol, empty search box is started."
    (interactive "P")
    (if u-arg
        (swiper)
      (swiper (selection-or-thing-at-point)))))


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
  :ensure t
  :commands (wgrep-setup)
  :hook (grep-setup . wgrep-setup)
  :bind (:map grep-mode-map
              ;; r/C-x C-q/C-c C-i to enter edit mode. C-x C-s to save, C-c C-k
              ("r" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-i" . wgrep-change-to-wgrep-mode))
  :config
  ;; Non-nil means do save-buffer automatically while wgrep-finish-edit.
  (setq wgrep-auto-save-buffer t))


;;;  ag.el
;; https://github.com/Wilfred/ag.el
;; http://agel.readthedocs.io/en/latest/index.html
;; https://github.com/ggreer/the_silver_searcher
;; http://yukihr.github.io/blog/2013/12/18/emacs-ag-wgrep-for-code-grep-search/
(use-package ag
  :ensure t
  :if (executable-find "ag")
  :commands (ag
             ;; *-files commands allow you to specify a PCRE pattern for file names to search in.
             ag-files
             ;; *-regexp commands allow you to specify a PCRE pattern for your search term.
             ag-regexp
             ;; *-project commands automatically choose the directory to search
             ag-project
             ag-project-files
             ag-project-regexp
             ;;
             ;; Search for file names
             ag-dired
             ag-dired-regexp
             ag-project-dired
             ag-project-dired-regexp)
  :bind (("s-a" . ag-files)
         ("C-s-a" . ag-project-files))
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
  (defun my--get-buffer-window-list-regexp (regexp)
    "Return list of windows whose buffer name matches regexp."
    ;; -filter in dash.el
    (-filter #'(lambda (window)
                 (string-match regexp
                               (buffer-name (window-buffer window))))
             (window-list)))
  (defun switch-to-ag (&optional _1 _2 _3)
    "Switch to a buffer named *ag ... Arguments are ignored."
    (select-window ; select ag buffer
     (car (my--get-buffer-window-list-regexp "^\\*ag "))))
  ;; Advice all user functions.
  (cl-loop for f in '(ag
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
    :ensure t
    ;; Nested within ag.el configuration.
    :demand
    :bind (:map ag-mode-map
                ("C-x C-q" . wgrep-change-to-wgrep-mode)
                ("C-c C-c" . wgrep-finish-edit))
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
  :ensure t
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
;;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind* (("C-," . er/expand-region)
          ("C-M-," . er/contract-region)
          :map my-key-map
          ("," . er/expand-region)
          ("M-," . er/contract-region)))


;;;
;;; plur.el
;; https://github.com/xuchunyang/plur
;; https://emacs.stackexchange.com/questions/27135/search-replace-like-feature-for-swapping-text/27170
;;
;; To replace “mouse” with “cat” and “mice” with “cats” using:
;; M-x plur-query-replace RET m{ouse,ice} RET cat{,s} RET
(use-package plur
  :ensure t
  :commands (plur-replace
             plur-query-replace))


;;;
;;; cmigemo (installed from Homebrew)
;; Used brew to install cmigemo
;; Used M-x list-package to install migemo.el (github)
;; Configured refering to: http://d.hatena.ne.jp/ground256/20111008/1318063872
;; Works by advising isearch-mode
;;
;; Mac-only configuration
(use-package migemo
  :ensure t
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
;;; rainbow-mode.el
;; Make strings describing colors appear in colors
;; http://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :ensure t
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
  :ensure t
  :demand
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1
             avy-goto-word-or-subword-1
             avy-isearch)
  :bind (("s-l" . avy-goto-line)
         ("C-'" . avy-goto-line)
         ("C-M-s" . avy-goto-char-timer)
         :map isearch-mode-map
         ("s-a" . avy-isearch)
         ("C-'" . avy-isearch))
  :config
  ;; Darken background in GUI only.
  (setq avy-background (display-graphic-p))
  ;; Highlight the first decision char with `avy-lead-face-0'.
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-highlight-first
  (setq avy-highlight-first t)
  ;; The default method of displaying the overlays.
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
  (setq avy-style 'at-full)
  ;; Keys to be used. Use a-z.
  (setq avy-keys (cl-loop for c from ?a to ?z collect c))
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
  (cl-loop for c from ?! to ?~ do (add-keys-to-avy "M-s-" c))
  (cl-loop for c from ?! to ?~ do (add-keys-to-avy "H-" c))
  (cl-loop for c from ?! to ?~ do (add-keys-to-avy "C-M-s-" c 'word)))


;;;  ace-window.el
;; Window selection using avy.el (no dependency on ace-jump-mode.el)
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :commands (ace-select-window
             ace-swap-window)
  :bind ("s-5" . ace-window))
