;; M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (material-modified)))
 '(custom-safe-themes t)
 '(package-selected-packages
   '(stream symbol-overlay pdf-tools major-mode-hydra lispy hercules wgrep-ag bash-completion magit-todos magit-gitflow counsel-projectile org-babel-eval-in-repl eval-in-repl window-number windresize viewer git-messenger git-timemachine magit git-gutter-fringe git-gutter flycheck-grammarly synosaurus synonymous osx-dictionary flyspell-popup csv-mode nov open-junk-file vlf recentf-ext projectile rainbow-delimiters paredit org-bullets org2blog org-ref org-plus-contrib esup command-log-mode manage-minor-mode tagedit graphviz-dot-mode grip-mode markdown-preview-mode stan-snippets flycheck-stan eldoc-stan company-stan stan-mode flycheck-cask flycheck-package flycheck-pos-tip hy-mode ein geiser slime-company demo-it elisp-lint package-lint assess flycheck buttercup nameless macrostep eros elisp-slime-nav xr smart-jump dumb-jump helm-gtags aggressive-indent irony-eldoc company-irony irony company-c-headers c-eldoc free-keys which-key sequential-command ddskk ace-window avy rainbow-mode migemo plur expand-region ibuffer-vc rg ag wgrep multiple-cursors anzu highlight-symbol helpful helm evil poly-R ess dired-recent dired-quick-sort async runner dired-filter dired-narrow dired-subtree dired-rsync diredfl reveal-in-osx-finder bm undo-tree super-save counsel ivy company-try-hard company-statistics auto-complete latex-math-preview company-math company-auctex auctex-latexmk auctex vterm clipetty xclip crontab-mode emamux prodigy tramp-term sudo-edit exec-path-from-shell poly-markdown elscreen-separate-buffer-list init-loader use-package))
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
