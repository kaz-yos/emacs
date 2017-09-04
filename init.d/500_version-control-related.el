;;; Version control configurations

;;;
;;; ediff for visual diff
;; Show in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Show them side by side
(setq ediff-split-window-function 'split-window-horizontally)

;;;
;;; Default VC system (turned off)
;; Do not use the default vc-mode (mode-line cleaner) 2014-02-14
;; http://qiita.com/acple@github/items/3709174ab24c5d82423a
(setq vc-handled-backends nil)
;; Turn off related hooks
(remove-hook 'find-file-hook   'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)



;;;
;;; In-buffer highlighting of changes

;;;  git-gutter.el
;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  ;; Use in CLI.
  :if (not (display-graphic-p))
  :diminish git-gutter-mode
  :config
  ;; Hook run after refreshing in `magit-refresh'.
  (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)
  ;; Activate everywhere.
  (global-git-gutter-mode))


;;;  git-gutter-fringe.el
;; https://github.com/syohex/emacs-git-gutter-fringe
(use-package git-gutter-fringe
  ;; Use in GUI.
  :if (display-graphic-p)
  :diminish git-gutter-mode
  :config
  ;; Keys
  (bind-key "A-p" 'git-gutter:previous-hunk)
  (bind-key "A-n" 'git-gutter:next-hunk)
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
  :commands (magit-status)
  :bind (("s-g" . my-magit-status)
         ("C-c g" . magit-status))
  :config
  ;; Avoid key binding conflict
  (define-key magit-status-mode-map (kbd "S-<tab>") 'magit-section-cycle)
  (define-key magit-status-mode-map (kbd "C-<tab>") nil)
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
    :init
    (setq magit-gitflow-popup-key "C-c f")
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))


;;;  git-timemachine.el
;; Use git-timemachine to browse historic versions of a file with p
;; (previous) and n (next).
(use-package git-timemachine
  :commands (git-timemachine-toggle
             git-timemachine))


;;;  github-browse-file.el
;; https://github.com/osener/github-browse-file
(use-package github-browse-file
  :commands (github-browse-file
             github-browse-file-blame
             github-browse-commit))


;;;  git-messenger.el
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :commands (git-messenger:popup-message
             git-messenger:popup-diff)
  :config
  (setq git-messenger:show-detail t))
