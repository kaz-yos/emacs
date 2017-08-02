;;;
;;; crosshairs.el: highlight current line/column using hl-line(+).el/col-highlight.el
;; http://www.emacswiki.org/emacs/CrosshairHighlighting
(use-package crosshairs
  :commands (crosshairs-mode)
  :config
  ;; (toggle-crosshairs-when-idle t) ; No need for crosshairs when idle
  ;; (col-highlight-set-interval 60)
  ;;
  ;; hl-line+.el: highlight current line only (no column)
  ;; http://www.emacswiki.org/emacs/HighlightCurrentLine#toc3
  ;; (require 'hl-line+)                ; required by crosshairs already
  ;; (toggle-hl-line-when-idle t)       ; turned on line highlight when idle
  ;; (toggle-hl-line-when-idle nil)     ; turned off line highlight when idle
  (hl-line-when-idle-interval 60)
  ;;
  ;; To customize the background color
  ;; (setq my-highlight-color "light goldenrod yellow")
  (setq my-highlight-color "dark red")
  (set-face-background 'hl-line my-highlight-color) ; Line color
  (set-face-background 'col-highlight my-highlight-color) ; Column color
  ;; (set-face-background 'hl-line "light goldenrod yellow")            ; Line color
  ;; (set-face-background 'col-highlight "light goldenrod yellow")      ; Column color
  ;; (set-face-background 'hl-line "lemon chiffon")                     ; Line color
  ;; (set-face-background 'col-highlight "lemon chiffon")               ; Column color
  )


;;;
;;; highligh-symbol for highlighting multiple occurences
;; http://nschum.de/src/emacs/highlight-symbol/
;; http://stackoverflow.com/questions/385661/emacs-highlight-all-occurences-of-a-word
(use-package highlight-symbol
  :bind ("C-." . highlight-symbol-at-point)
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
    (recenter))
  ;;
  (global-set-key (kbd "C-\}") 'my-highlight-symbol-next)
  (global-set-key (kbd "C-\{") 'my-highlight-symbol-prev)
  (global-set-key (kbd "A-]") 'my-highlight-symbol-next)
  (global-set-key (kbd "A-[") 'my-highlight-symbol-prev))


;;;
;;; anzu.el 2014-02-01
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
;; http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(use-package anzu
  :demand
  :config
  (global-anzu-mode +1)
  (setq anzu-mode-lighter "")
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 1)
  ;;
  ;; Define a large face (also used for multiple-cursors.el)
  ;; This was done in custom-set-faces.
  ;;
  ;; (global-set-key (kbd "C-c r") 'anzu-query-replace)
  ;; (global-set-key (kbd "C-c R") 'anzu-query-replace-regexp)
  )


;;;
;;; multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el
;; http://ongaeshi.hatenablog.com/entry/20121205/1354672102 (for a similar package)
;; http://emacsrocks.com/e13.html (video)
;; http://rubikitch.com/2014/11/10/multiple-cursors/
;;
(use-package multiple-cursors
  :demand ; otherwise mode-line override does not work
  :init
  ;; Where to save command list
  (setq mc/list-file "~/.emacs.d/mc-lists.el")
  ;;
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
;;; phi-search.el
;; another incremental search command, compatible with “multiple-cursors”
;; https://github.com/zk-phi/phi-search
;; https://www.youtube.com/watch?v=JSTO674y6Hcp
;; http://rubikitch.com/2014/11/11/phi-search/
(use-package phi-search
  :commands (phi-search
             phi-search-backward))



;;; SWIPER-RELATED
;;;  swiper.el
;; https://github.com/abo-abo/swiper
;; http://pragmaticemacs.com/emacs/dont-search-swipe/
;; http://pragmaticemacs.com/emacs/search-or-swipe-for-the-current-word/
(use-package swiper
  :commands (swiper
             swiper-at-point)
  ;; :bind (("H-s" . swiper-at-point)
  ;;        ("C-s-s" . swiper)
  ;;        ;; Add bindings to isearch-mode
  ;;        :map isearch-mode-map
  ;;        ("s-s" . swiper-from-isearch))
  :init
  ;; Newly defined
  (defun swiper-at-point ()
    "Custom function to pick up a thing at a point for swiper

If a selected region exists, it will be searched for by swiper
If there is a symbol at the current point, its textual representation is
searched. If there is no symbol, empty search box is started."
    (interactive)
    (swiper (cond
             ;; If there is selection use it
             ((and transient-mark-mode mark-active
                   (not (eq (mark) (point))))
              (buffer-substring-no-properties (mark) (point)))
             ;; Otherwise, use symbol at point or empty
             (t (format "%s"
                        (or (thing-at-point 'symbol)
                            ""))))))
  ;;
  :config)
;;
;;;  swiper-helm.el
;; https://github.com/abo-abo/swiper-helm
(use-package swiper-helm
  :commands (swiper-helm
             swiper-helm-from-isearch)
  :bind (("s-s" . swiper-helm-at-point)
         ("C-s-s" . swiper-helm)
         :map isearch-mode-map
         ("s-s" . swiper-helm-from-isearch))
  :init
  ;; Newly defined
  (defun swiper-helm-at-point ()
    "Custom function to pick up a thing at a point for swiper-helm

If a selected region exists, it will be searched for by swiper-helm
If there is a symbol at the current point, its textual representation is
searched. If there is no symbol, empty search box is started."
    (interactive)
    (swiper-helm (cond
                  ;; If there is selection use it
                  ((and transient-mark-mode mark-active
                        (not (eq (mark) (point))))
                   (buffer-substring-no-properties (mark) (point)))
                  ;; Otherwise, use symbol at point or empty
                  (t (format "%s"
                             (or (thing-at-point 'symbol)
                                 ""))))))
  ;; Configuration
  :config)


;;;
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
;;
;; ;; Modern version (incomplete)
;; ;; Define function
;; (defun isearch-selection-advice (old-fun &optional regexp-p no-recursive-edit)
;;   "Use selection for isearch"
;;   (interactive)
;;   (old-fun (cond
;;             ;; If there is selection use it
;;             ((and transient-mark-mode mark-active (not (eq (mark) (point))))
;;              (buffer-substring-no-properties (mark) (point)))
;;             ;; Otherwise, use symbol at point or empty
;;             (t (format "%s"
;;                        (or (thing-at-point 'symbol)
;;                            ""))))))
;; ;; Define around advice
;; (advice-add :around )


;;;
;;; MULTIPLE FILE GREP AND EDIT RELATED
;;;  color-moccur.el/moccur-edit.el
;; Requires color-moccur.el (elpa)
;; http://www.bookshelf.jp/elc/moccur-edit.el
;; http://d.hatena.ne.jp/higepon/20061226/1167098839
;; http://d.hatena.ne.jp/sandai/20120304/p2
(use-package color-moccur
  :commands (moccur-grep
             moccur-grep-find)
  :bind (("s-m" . moccur-grep)
         ;; https://github.com/jwiegley/use-package#the-basics
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  ;;
  :init
  (setq isearch-lazy-highlight t)
  ;;
  :config
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


;;;  wgrep
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


;;;  ag.el/wgrep-ag.el
;; https://github.com/Wilfred/ag.el
;; http://agel.readthedocs.io/en/latest/index.html
;; https://github.com/ggreer/the_silver_searcher
;; http://yukihr.github.io/blog/2013/12/18/emacs-ag-wgrep-for-code-grep-search/
;; ag.el
(use-package ag
  :commands (ag
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp)
  :bind (("s-a" . ag))
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
  ;; Editing via wgrep-ag.el
  (use-package wgrep-ag
    :config
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    ;; r/C-x C-q/C-c C-i to enter edit mode. C-x C-s to save, C-c C-k
    (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)
    (define-key ag-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
    (define-key ag-mode-map (kbd "C-c C-i") 'wgrep-change-to-wgrep-mode)))

;;;  rg.el
;; https://github.com/dajva/rg.el
(use-package rg
  :commands (rg)
  ;;
  :config
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
  :commands (embrace-commander)
  :bind
  ("H-e" . embrace-commander)
  ("s-e" . embrace-commander))


;;;
;;; cmigemo (installed from Homebrew)
;; Used brew to install cmigemo
;; Used M-x list-package to install migemo.el (github)
;; Configured refering to: http://d.hatena.ne.jp/ground256/20111008/1318063872
;; Works by advising isearch-mode
;;
;; Mac-only configuration
(when (executable-find "/usr/local/bin/cmigemo")
  (use-package migemo
    :commands (migemo-init)
    :init
    (add-hook 'after-init-hook 'migemo-init)
    :config
    (setq migemo-command "/usr/local/bin/cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-regex-dictionary nil)
    ;; Advising
    (migemo-init)))


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
;; PaleVioletRed1 orange1 turquoise4


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
             avy-goto-word-or-subword-1)
  :config
  ;; Darken background
  (setq avy-background t)
  ;;
  ;; Time out for *-timer functions
  (setq avy-timeout-seconds 0.3)
  ;; Key bind for regular avy timer
  (global-set-key (kbd "C-M-s") 'avy-goto-char-timer)
  ;; Modified version of avy-goto-char-timer
  (defun avy-goto-char-timer-mod (char1 &optional arg)
    "Read one or many consecutive chars and jump to the first one.

This version takes the first character as an argument.
The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
        ;; avy--process: Select one of CANDIDATES
        (avy--process
         ;; Read as many chars as possible and return their occurences (modified)
         (avy--read-candidates-mod (char-to-string char1))
         ;; avy--style-fn: Transform STYLE symbol to a style function.
         (avy--style-fn avy-style)))))
  ;;
  ;; Modified version of avy--read-candidates compatible with avy-goto-char-timer-mod
  (defun avy--read-candidates-mod (str1)
    "Read as many chars as possible and return their occurences.

This version takes the first string element as an argument.
At least one char must be read, and then repeatedly one next char
may be read if it is entered before `avy-timeout-seconds'.  `DEL'
deletes the last char entered, and `RET' exits with the currently
read string immediately instead of waiting for another char for
`avy-timeout-seconds'.
The format of the result is the same as that of `avy--regex-candidates'.
This function obeys `avy-all-windows' setting."
    ;; string should start with str1 given as argument
    (let ((str str1) char break overlays regex)
      (unwind-protect
          ;; BODYFORM
          (progn
            ;; Loop while no break or no time out
            ;; Allow going to body with a nil char (no key entry other than str1 arg)
            ;; Don't forget to update nil char to str1 if this happens.
            (while (or (not char)
                       ;; Second pass
                       (and (not break)
                            ;; set char as a character code of key entry
                            (setq char
                                  ;; Read a character from the command input and return as a number.
                                  (read-char
                                   ;; PROMPT: what to display as a prompt (str is non-empty)
                                   (format "char%s: " (format " (%s)" str))
                                   ;; INHERIT-INPUT-METHOD
                                   t
                                   ;; SECONDS: maximum number of seconds to wait for input
                                   avy-timeout-seconds))))
              ;; WHILE BODY
              ;; Unhighlight
              (dolist (ov overlays)
                (delete-overlay ov))
              (setq overlays nil)
              ;; Handling of special key entry
              (cond
               ;; Ignore char if is is nil, and use str = str1 only.
               ((not char)
                ;; Update char to the initial str = str1 character code to avoid
                ;; an infinite loop at while (or (not char) ...)
                (setq char (string-to-char str1)))
               ;; Handle RET
               ((= char 13)
                (setq break t))
               ;; Handle DEL
               ((= char 127)
                (let ((l (length str)))
                  (when (>= l 1)
                    (setq str (substring str 0 (1- l))))))
               (t
                ;; This appends char as a string to str.
                (setq str (concat str (list char)))))
              ;;
              ;; Highlight
              (when (>= (length str) 1)
                (let ((case-fold-search
                       (or avy-case-fold-search (string= str (downcase str))))
                      found)
                  (avy-dowindows current-prefix-arg
                    (dolist (pair (avy--find-visible-regions
                                   (window-start)
                                   (window-end (selected-window) t)))
                      (save-excursion
                        (goto-char (car pair))
                        (setq regex (regexp-quote str))
                        (while (re-search-forward regex (cdr pair) t)
                          (unless (get-char-property (1- (point)) 'invisible)
                            (let ((ov (make-overlay
                                       (match-beginning 0)
                                       (match-end 0))))
                              (setq found t)
                              (push ov overlays)
                              (overlay-put
                               ov 'window (selected-window))
                              (overlay-put
                               ov 'face 'avy-goto-char-timer-face)))))))
                  ;; No matches at all, so there's surely a typo in the input.
                  (unless found (beep)))))
            (nreverse (mapcar (lambda (ov)
                                (cons (cons (overlay-start ov)
                                            (overlay-end ov))
                                      (overlay-get ov 'window)))
                              overlays)))
        ;; UNWINDFORMS
        (dolist (ov overlays)
          (delete-overlay ov)))))
  ;;
  ;; avy version of one-step activation
  ;; http://d.hatena.ne.jp/rkworks/20120520/1337528737
  ;; https://github.com/cjohansen/.emacs.d/commit/65efe88
  (defun add-keys-to-avy (prefix c &optional mode)
    (define-key global-map
      (read-kbd-macro (concat prefix (string c)))
      `(lambda ()
         (interactive)
         (funcall (cond
                   ;; Word beginning
                   ((eq ',mode 'word)  #'avy-goto-word-1)
                   ;; Modified timer
                   ((eq ',mode 'timer) #'avy-goto-char-timer-mod)
                   ;; Anywhere
                   (t                  #'avy-goto-char))
                  ,c))))
  ;;
  ;; Assing key bindings for all characters
  ;;
  ;; (loop for c from ?! to ?~ do (add-keys-to-avy "M-s-" c 'timer))
  ;; eg, M-s-x will activate (avy-goto-char ?x), ie, all occurrence of x
  (loop for c from ?! to ?~ do (add-keys-to-avy "M-s-" c))
  ;; (loop for c from ?! to ?~ do (add-keys-to-avy "C-M-s-" c))
  ;; eg, A-s-x will activate (avy-goto-word-1 ?x), ie, all words starting with x
  (loop for c from ?! to ?~ do (add-keys-to-avy "C-M-s-" c 'word))
  )
;;
;;
;;;  ace-window.el
;; Window selection using avy.el (no dependency on ace-jump-mode.el)
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :commands (ace-select-window
             ace-swap-window)
  :bind ("s-5" . ace-window)
  :config
  (global-set-key (kbd "s-5") 'ace-window)
  ;;
  ;; Face configuration (Do it in init-customize.el)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Attribute-Functions.html
  ;; (set-face-attribute FACE FRAME &rest ARGS)
  ;; FRAME nil for all existing frames.
  ;; (set-face-attribute 'aw-leading-char-face nil
  ;;                     :foreground "red" :height 5.0)
  )
;;
;;
;;;  ace-jump-helm-line.el
;; Depends on avy.el not ace-jump-mode.el
;; https://github.com/cute-jumper/ace-jump-helm-line
(use-package ace-jump-helm-line
  :commands (ace-jump-helm-line)
  :config
  (eval-after-load "helm"
    '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line)))
