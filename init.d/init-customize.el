;; M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(custom-enabled-themes ,(if (display-graphic-p)
                              ''(material-modified)
                            ''(material-modified)))
 '(custom-safe-themes t)
 '(package-selected-packages
   (quote
    (vterm clipetty xclip demo-it flycheck-grammarly crontab-mode stan-mode stan-snippets company-stan eldoc-stan multiple-cursors which-key ac-stan elscreen-buffer-group magit-todos dired-filter taskrunner assess grip-mode xr cask elisp-lint flycheck-package package-lint buttercup c-eldoc poly-R poly-markdown poly-org ein git-messenger tagedit eval-in-repl org-babel-eval-in-repl dired-recent ibuffer-vc emamux org2blog company-anaconda company-math org-plus-contrib org-ref esup dired-rsync eros diredfl smart-jump window-purpose pdf-tools nameless counsel-projectile avy-migemo material-theme git-gutter-fringe company-irony embrace org-bullets ob-async plur synosaurus irony-eldoc irony sudo-edit rg synonymous helpful dumb-jump helm-gtags company-c-headers magit-gitflow org-edit-latex dired-quick-sort magit dired-narrow ddskk tramp-term slime-company free-keys command-log-mode color-moccur langtool osx-dictionary zoom-window paredit prodigy counsel elscreen-separate-buffer-list super-save eldoc-extension expand-region aggressive-indent mu4e-alert elscreen windresize window-number window-layout wgrep-ag vlf viewer use-package tuareg ssh sql-indent sml-mode shell-command runner reveal-in-osx-finder rainbow-mode rainbow-delimiters racket-mode polymode nose noflet markdown-mode manage-minor-mode log4e latex-math-preview latest-clojure-libraries json-rpc js3-mode js-comint init-loader inflections hydra hy-mode highlight-symbol graphviz-dot-mode gntp github-browse-file git-timemachine ghc fuzzy flyspell-popup flycheck-pos-tip find-file-in-project exec-path-from-shell evil-surround evil ess-R-object-popup es-windows es-lib elisp-slime-nav edn dired-subtree csv-mode company-try-hard company-statistics company-auctex column-marker clojure-snippets cider-tracing bm bash-completion auto-complete-auctex auctex-latexmk anzu ag ace-window ace-jump-helm-line ac-math ac-js2 ac-geiser ac-cider)))
 '(safe-local-variable-values
   (quote
    ((checkdoc-minor-mode . 1)
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (unless
               (derived-mode-p
                (quote emacs-lisp-mode))
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (checkdoc-minor-mode . t))))
 '(tramp-syntax (quote default) nil (tramp)))
;;
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
