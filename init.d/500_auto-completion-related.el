;;; 500_auto-completion-related.el ---               -*- lexical-binding: t; -*-

;;;
;;; hippie-expand (built-in)
;; hippie-expand instead of less functional dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)


;;;
;;; AUTO-COMPLETE
;;;  auto-complete.el, auto-complete-config.el, fuzzy.el, popup.el downloaded from below URL
;; https://github.com/auto-complete/auto-complete
;; http://cx4a.org/software/auto-complete/manual.html
(use-package auto-complete
  :commands (auto-complete
             auto-complete-mode)
  ;;
  :init
  ;; Configure default sources
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-filename
                             ac-source-words-in-same-mode-buffers))
  ;; Added to avoid ac-modes not defined errors on ess startup.
  ;; Major modes `auto-complete-mode' can run on.
  (setq ac-modes
        '(emacs-lisp-mode
          lisp-mode lisp-interaction-mode
          slime-repl-mode
          nim-mode c-mode cc-mode c++-mode objc-mode swift-mode go-mode
          java-mode malabar-mode clojure-mode clojurescript-mode  scala-mode
          scheme-mode
          ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode
          perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode
          ecmascript-mode javascript-mode js-mode js-jsx-mode js2-mode js2-jsx-mode
          coffee-mode php-mode css-mode scss-mode less-css-mode
          elixir-mode
          makefile-mode sh-mode fortran-mode f90-mode ada-mode
          xml-mode sgml-mode web-mode
          ts-mode
          sclang-mode
          verilog-mode
          qml-mode
          apples-mode))
  ;;
  :config
  ;; Auto-complete for ESS configuration
  ;; http://www.emacswiki.org/emacs/ESSAuto-complete
  (setq
   ;; Limit number of candidates
   ;; ac-candidate-limit nil
   ;;
   ;; Number of letters before ac kicks in
   ac-auto-start 1
   ;; Delay to completions will be available.
   ac-delay 0.1
   ;; 0.1 sec before menu appears
   ac-auto-show-menu 0.2
   ;; Show menu if 2+ candidates
   ac-candidate-menu-min 1
   ;; 20 candidates at a time
   ac-menu-height 20
   ;; Where to disable ac
   ;; http://stackoverflow.com/questions/17309773/emacs-autocomplete-inside-python-string
   ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
   ;; auto-complete everywhere, even within quotes, comments
   ;; ac-disable-faces nil
   ac-disable-faces '(font-lock-comment-face)
   ;; Treat cases strictly by nil or smartly by 'smart
   ac-ignore-case nil
   ;;
   ;; Pop up help
   ac-use-quick-help t
   ac-quick-help-delay 1.0
   ac-quick-help-prefer-pos-tip t)
  ;;
  ;; Key configuration
  ;; Non-nil means a special keymap `ac-menu-map' on completing menu will be used.
  ;; http://cx4a.org/software/auto-complete/manual.html#Not_to_complete_automatically
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n")     'ac-next)
  (define-key ac-menu-map (kbd "C-p")     'ac-previous)
  (define-key ac-menu-map (kbd "<tab>")   'ac-next)
  (define-key ac-menu-map (kbd "<S-tab>") 'ac-previous)
  ;;
  ;; http://www.emacswiki.org/emacs/ESSAuto-complete
  (define-key ac-completing-map (kbd "<tab>")    'ac-complete)
  (define-key ac-completing-map (kbd "<return>") 'ac-complete)
  ;;
  ;; Trigger key
  ;; http://cx4a.org/software/auto-complete/manual.html#Trigger_Key
  (ac-set-trigger-key "TAB")
  ;;
  ;; If you are using 'flyspell' you might want to activate the workaround
  ;; http://www.emacswiki.org/emacs/AutoComplete#toc6
  (ac-flyspell-workaround)
  ;;
  ;; popup.el
  ;; https://github.com/auto-complete/popup-el (called automatically)
  ;; Prevent broken popup
  ;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
  (setq popup-use-optimized-column-computation nil))


;;;
;;; COMPANY-MODE (COMPlete ANYthing)
;;;  company.el
;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io
;; http://www.emacswiki.org/CompanyMode
;; http://qiita.com/syohex/items/8d21d7422f14e9b53b17
;; http://qiita.com/sune2/items/b73037f9e85962f5afb7
;; http://emacs.stackexchange.com/questions/3654/filename-completion-using-company-mode
;; http://blog.binchen.org/posts/emacs-auto-completion-for-non-programmers.html
;; http://comments.gmane.org/gmane.emacs.ess.general/9037
;;
(use-package company
  :diminish (company-mode)
  :commands (global-company-mode)
  :bind (("A-<tab>" . company-complete)
         ("A-i" . company-complete)
         ;;
         ;; Keymap that is enabled during an active completion.
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ;;
         ;; Keymap used for incrementally searching the completion candidates.
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  ;;
  :init
  ;; company-mode everywhere
  (add-hook 'after-init-hook 'global-company-mode)
  ;; except in these modes
  ;; (setq company-global-modes '(not eshell-mode ielm-mode))
  ;;
  :config
  ;; Delay time
  (setq company-idle-delay 0.3)
  ;; 2 letters before completion kicks in
  (setq company-minimum-prefix-length 2)
  ;; Wrap after the final candidate
  (setq company-selection-wrap-around t)
  ;; Now donwcasing
  (setq company-dabbrev-downcase nil)
  ;;
  ;; Avoid completing Japanese letters in company-dabbrev
  ;; http://qiita.com/wktkshn/items/3ac46671d1c242a59f7e
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html
  ;; http://emacs.rubikitch.com/syntax-table-beginner/
  (defun edit-category-table-for-company-dabbrev (&optional table)
    "Create a syntax table consisting only of ascii letters for company dabbrev."
    (define-category ?s "word constituents for company-dabbrev" table)
    (let ((i 0))
      (while (< i 128)
        (if (equal ?w (char-syntax i))
            (modify-category-entry i ?s table)
          (modify-category-entry i ?s table t))
        (setq i (1+ i)))))
  (edit-category-table-for-company-dabbrev)
  (setq company-dabbrev-char-regexp "\\cs")
  ;;
  ;; Backends (Adding globally is also ok; backends are context-aware)
  ;; http://emacs.stackexchange.com/questions/17537/best-company-backends-lists
  ;; Emacs lisp (one of default backends; somehow not included originally)
  ;; (add-to-list 'company-backends 'company-elisp) ; adding globally
  ;; Add locally
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook '(lambda ()
                      (add-to-list (make-local-variable 'company-backends)
                                   'company-elisp)))))

;;;  company-statistics.el
;; Sort completion candidates by previous completion choices
;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :init
  (add-hook 'company-mode-hook 'company-statistics-mode)
  ;;
  :commands (company-statistics-mode)
  ;;
  :config
  (setq company-statistics-file (concat user-emacs-directory
                                        "company-statistics-cache"
                                        "_"
                                        (system-name-sans-domain)
                                        ".el")))

;;;  company-quickhelp.el
;; Documentation popup for Company
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :disabled t
  :config
  (company-quickhelp-mode 1))

;;;  company-try-hard.el
;; https://github.com/Wilfred/company-try-hard
(use-package company-try-hard
  :commands (company-try-hard)
  :bind (("s-i" . company-try-hard)
         ("s-t" . company-try-hard)
         :map my-key-map
         ("t" . company-try-hard)))

;;;  company-ngram.el
;; https://github.com/kshramt/company-ngram
(use-package company-ngram
  :disabled t
  :config
  ;; ~/data/ngram/*.txt are used as data
  (setq company-ngram-data-dir
        (concat user-emacs-directory
                "~/data/ngram"))
  ;; company-ngram supports python 3 or newer
  (setq company-ngram-python "python3")
  ;; Initiate use
  (company-ngram-init)
  (cons 'company-ngram-backend company-backends)
  ;; save the cache of candidates
  (run-with-idle-timer 7200 t
                       (lambda ()
                         (company-ngram-command "save_cache"))))


;;;
;;; IVY-RELATED

;;;  ivy.el
;; https://github.com/abo-abo/swiper
;; http://oremacs.com/swiper/
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
;; https://www.reddit.com/r/emacs/comments/5453d4/what_does_your_ivyswiper_configuration_look_like/
(use-package ivy
  :diminish ivy-mode
  :config
  ;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
  ;; Add recent files and bookmarks to the ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; Set this to "(%d/%d) " to display both the index and the count.
  (setq ivy-count-format "%d/%d ")
  ;; Number of lines for the minibuffer window.
  (setq ivy-height 20)
  ;;
  ;; Out of order matching
  ;; https://oremacs.com/2015/05/23/swiper-0.5.0/
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  ;;
  ;; Additional keys
  ;; ivy-immediate-done finishes without completion
  (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)
  ;; Activate
  (ivy-mode 1))

;;;  counsel.el
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html#org2d4e119
;; https://oremacs.com/2015/04/09/counsel-completion/
;; http://cestlaz.github.io/posts/using-emacs-6-swiper/#.WYKJHa2ZOEI
;; https://oremacs.com/2017/11/18/dired-occur/
(use-package counsel
  :commands (counsel-ag
             counsel-rg
             counsel-git-grep)
  :bind (("s-w" . counsel-ag-at-point)
         ("C-s-w" . counsel-ag)
         ("C-c C-w" . counsel-ag))
  ;;
  :config
  ;; Project directory advise.
  (defun counsel-ag-arg2-to-project-root (args)
    "Swap the second argument to the project root directory.

ARGS is a list of arguments."
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
    `(,(car args)
      ;; New second element
      ,(projectile-project-root)
      ,@(cddr args)))
  ;;
  ;; (advice-add #'counsel-ag
  ;;             :filter-args
  ;;             #'counsel-ag-arg2-to-project-root)
  ;;
  (defun counsel-ag-at-point (u-arg)
    "counsel-ag with at-point and project root enhancement

The initial string is produced by selection-or-thing-at-point.
The directory is detected by projectile-project-root."
    (interactive "P")
    (if u-arg
        (counsel-ag)
      (counsel-ag (selection-or-thing-at-point)
                  ;; This will be replaced by the above advice anyway.
                  (projectile-project-root))))
  ;;
  (defun counsel-rg-at-point (u-arg)
    "counsel-rg with at-point and project root enhancement

The initial string is produced by selection-or-thing-at-point.
The directory is detected by projectile-project-root."
    (interactive "P")
    (if u-arg
        (counsel-rg)
      (counsel-rg (selection-or-thing-at-point)
                  ;; This will be replaced by the above advice anyway.
                  (projectile-project-root))))
  ;; https://github.com/abo-abo/swiper/issues/66
  (defun counsel-git-grep-at-point (u-arg)
    "counsel-git-grep with at-point enhancement

The initial string is produced by selection-or-thing-at-point."
    (interactive "P")
    (if u-arg
        (counsel-git-grep)
      (counsel-git-grep nil
                        (selection-or-thing-at-point)))))


;;;
;;; git-complete.el
;; Yet another completion engine powered by git grep
;; https://github.com/zk-phi/git-complete
(use-package git-complete
  :commands (git-complete)
  :bind (("A-c" . git-complete)
         ("H-c" . git-complete)
         :map my-key-map
         ("G" . git-complete)))


;;;
;;; Handling of the tab completion buffer 2014-02-03
;; http://stackoverflow.com/questions/6458220/automatically-close-emacs-shell-mode-tab-completion-buffer
(defun delete-completion-window-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*")))
  output)
(add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer)
;; http://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening
(add-hook 'minibuffer-exit-hook 'delete-completion-window-buffer)



;;; popwin.el
;; https://github.com/m2ym/popwin-el
;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
(use-package popwin
  :disabled t
  :config
  ;; Where to show
  (setq popwin:popup-window-position 'left)
  ;; width 20% of frame width
  (setq popwin:popup-window-width 0.2)
  ;; height 30% of frame height
  (setq popwin:popup-windowheightwidth 0.3)
  ;;
  ;; Buffers under control (default minus help-mode)
  (setq popwin:special-display-config
        '(;; ("*Completions*")
          (completion-list-mode :noselect t)
          ;; (compilation-mode :noselect t)
          (grep-mode :noselect t)
          (occur-mode :noselect t)
          ("*Pp Macroexpand Output*" :noselect t)
          ("*Shell Command Output*")
          ("*vc-diff*")
          ("*vc-change-log*")
          (" *undo-tree*" :width 60 :position right)
          ("^\\*anything.*\\*$" :regexp t)
          ("*slime-apropos*")
          ("*slime-macroexpansion*")
          ("*slime-description*")
          ("*slime-compilation*" :noselect t)
          ("*slime-xref*")
          (sldb-mode :stick t)
          (slime-repl-mode)
          (slime-connection-list-mode)))
  ;;
  ;; Add buffers under control
  ;; http://aikotobaha.blogspot.com/2013/04/popwinel.html
  ;; Completions
  ;; (push '("*Completions*") popwin:special-display-config)
  )
