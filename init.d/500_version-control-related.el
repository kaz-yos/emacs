;;; 500_version-control-related.el ---               -*- lexical-binding: t; -*-

;;;
;;; ediff-wind.el
;; ediff for visual diff
(use-package ediff-wind
  :config
  ;; Show in the same frame
  ;; https://www.gnu.org/software/emacs/manual/html_node/ediff/Window-and-Frame-Configuration.html
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Show them side by side
  ;; https://www.gnu.org/software/emacs/manual/html_node/ediff/Miscellaneous.html
  (setq ediff-split-window-function 'split-window-horizontally))


;;;
;;; In-buffer highlighting of changes
;;;  git-gutter.el
;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  :ensure t
  ;; Use in CLI.
  :if (not (display-graphic-p))
  :config
  ;; No lighter (This variable is defined in git-gutter.el).
  (setq git-gutter:lighter "")
  ;; Hook run after refreshing in `magit-refresh'.
  (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)
  ;; Activate everywhere.
  (global-git-gutter-mode))
;;;  git-gutter-fringe.el
;; https://github.com/syohex/emacs-git-gutter-fringe
(use-package git-gutter-fringe
  :ensure t
  ;; Use in GUI.
  :if (display-graphic-p)
  :config
  ;; Keys
  (bind-key "A-p" 'git-gutter:previous-hunk)
  (bind-key "A-n" 'git-gutter:next-hunk)
  ;; No lighter (This variable is defined in git-gutter.el).
  (setq git-gutter:lighter "")
  ;; On the right.
  (setq git-gutter-fr:side 'right-fringe)
  ;; Hook run after refreshing in `magit-refresh'.
  (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)
  ;; Activate everywhere.
  (global-git-gutter-mode))


;;;
;;; GIT-RELATED
;;;  magit.el
;; e for ediff!
;; Magit User Manual: http://magit.github.io/magit/magit.html
;; emacs wiki magit: http://www.emacswiki.org/emacs/Magit
;; emacs wiki git: http://www.emacswiki.org/emacs/Git
;; http://gom.hatenablog.com/entry/20090524/1243170341
;; Meet Magit on Vimeo: http://vimeo.com/2871241
;; git real basics: http://xahlee.info/linux/git.html
;; magit tutorial: http://ergoemacs.org/emacs/emacs_magit-mode_tutorial.html
;; http://qiita.com/nishikawasasaki/items/f690ee08f6a32d9d03fa
(use-package magit
  :ensure t
  :commands (magit-status
             magit-init)
  :bind (("s-g" . my-magit-status)
         ("C-c g". my-magit-status)
         :map my-key-map
         ("g" . my-magit-status))
  :config
  ;; Avoid key binding conflict
  (define-key magit-status-mode-map (kbd "S-<tab>") 'magit-section-cycle)
  (define-key magit-status-mode-map (kbd "C-<tab>") nil)
  ;;
  ;; Do not show recent commits
  ;; https://github.com/magit/magit/issues/3230
  ;; https://github.com/magit/magit/issues/3147
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpulled-from-upstream
                          'replace)
  ;;
  (setq git-commit-mode-hook '(turn-on-flyspell
                               (lambda ()
                                 (set-fill-column 1024))))
  ;;
  ;; Show hash
  (setq magit-status-show-hashes-in-headers t)
  ;;
  ;; Word-level diff
  ;; https://magit.vc/manual/magit/Diff-options.html
  ;; http://html-color-codes.info
  (setq magit-diff-refine-hunk t)
  ;;
  ;; Just save everything when invoked
  (setq magit-save-some-buffers 'dontask)
  ;;
  (defun my-magit-status ()
    "Delete whitespaces, save, and magit-status"
    (interactive)
    ;; Do this only in a file associated buffer that's writable.
    ;; Avoids errors in dired or read-only buffers.
    (when (and (buffer-file-name)
               (not buffer-read-only))
      (delete-trailing-whitespace)
      (save-buffer))
    ;; Finally call magit-status
    (magit-status))
  ;; Improve cursor movement when using n
  (advice-add 'magit-section-forward
              :after 'my-recenter-top)
  (advice-add 'magit-section-forward-sibling
              :after 'my-recenter-top)
  ;;
;;;   magit-gitflow.el
  ;; https://github.com/jtatarik/magit-gitflow
  ;; Need to install gitflow-avh (brew install git-flow-avh).
  ;; https://github.com/petervanderdoes/gitflow-avh
  (use-package magit-gitflow
    :ensure t
    :init
    (setq magit-gitflow-popup-key "C-c f")
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
  ;;
;;;   magit-todos.el
  ;; https://github.com/alphapapa/magit-todos
  (use-package magit-todos
    :ensure t
    ;; Do not hook. Just load once.
    ;; https://github.com/alphapapa/magit-todos/issues/28
    ))


;;;  abridge-diff.el
;; https://github.com/jdtsmith/abridge-diff
(use-package abridge-diff
  :ensure t
  :after magit
  :config
  (abridge-diff-mode 1))


;;;  git-timemachine.el
;; Use git-timemachine to browse historic versions of a file with p
;; (previous) and n (next).
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
             git-timemachine))


;;;  git-messenger.el
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :ensure t
  :commands (git-messenger:popup-message
             git-messenger:popup-diff)
  :config
  (setq git-messenger:show-detail t))
