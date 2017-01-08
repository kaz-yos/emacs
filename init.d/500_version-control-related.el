;;; Version control configurations


;;; ediff for visual diff
;; Show in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Show them side by side
(setq ediff-split-window-function 'split-window-horizontally)


;;; Default VC system (turned off)
;; Do not use the default vc-mode (mode-line cleaner) 2014-02-14
;; http://qiita.com/acple@github/items/3709174ab24c5d82423a
(setq vc-handled-backends nil)
;; Turn off related hooks
(remove-hook 'find-file-hook   'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
;;
;; ;; modeline-git-branch.el (auto-install) 2014-02-14. Switch not fast enough.
;; (require 'modeline-git-branch)
;; (modeline-git-branch-mode 1)


;;;
;;; Git
;;; magit.el
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
  ;;
  (define-key magit-status-mode-map (kbd "S-<tab>") 'magit-section-cycle)
  (define-key magit-status-mode-map (kbd "C-<tab>") nil)
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
    ;; Invoke the true worker function (change tramp scp because it's slow)
    (magit-status)
    ;; (magit-status (replace-regexp-in-string "^/scp"
    ;;                                         "/ssh"
    ;;                                         default-directory))
    )
  ;; Improve cursor movement when using n
  (advice-add 'magit-section-forward
              :after 'my-recenter-top)
  (advice-add 'magit-section-forward-sibling
              :after 'my-recenter-top)
  )


;;; git-gutter-fringe+ (fringe version. depends on git-gutter+) 2014-02-02
;; Does not work in .emacs.d (not elisp in general) 2014-03-01
;; https://github.com/nonsequitur/git-gutter-fringe-plus
;; fringe-helper.el is required.
(use-package git-gutter-fringe+
  :init
  ;; Configure fringe for git-gutter 2014-02-02
  ;; http://stackoverflow.com/questions/11373826/how-to-disable-fringe-in-emacs
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fringes.html
  (set-fringe-mode '(0 . 1))
  (fringe-mode)
  ;; Mute the mode-line
  (setq git-gutter+-lighter "")
  ;; Show on the right side
  (setq git-gutter-fr+-side 'right-fringe)
  ;;
  :config
  ;; active everywhere
  (global-git-gutter+-mode)
  ;;
  ;; Moving between hunks
  (global-set-key (kbd "A-p") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "A-n") 'git-gutter+-next-hunk)
  ;;
  ;; Enhance emacs-git-gutter with ivy-mode
  ;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
  ;; Helper
  (defun my-reshape-git-gutter (gutter)
    "Re-shape gutter for `ivy-read'."
    (let* ((lineno (aref gutter 3))
           line)
      (save-excursion
        (goto-line lineno)
        (setq line (buffer-substring (line-beginning-position)
                                     (line-end-position))))
      ;; build (key . lineno)
      (cons (format "%s %d: %s"
                    (if (eq 'deleted (aref gutter 1)) "-" "+")
                    lineno
                    (replace-regexp-in-string "^[ \t]*" "" line))
            lineno)))
  ;; User function
  (defun my-goto-git-gutter ()
    (interactive)
    (if git-gutter+-diffinfos
        (let* ((collection (mapcar 'my-reshape-git-gutter
                                   git-gutter+-diffinfos)))
          (ivy-read "git-gutters:"
                    collection
                    :action (lambda (lineno)
                              (goto-line lineno))))
      (message "NO git-gutters!"))))
;;
;;
;;; git-timemachine.el
;; Use git-timemachine to browse historic versions of a file with p
;; (previous) and n (next).
(use-package git-timemachine
  :commands (git-timemachine-toggle
             git-timemachine))
;;
;;
;;; github-browse-file.el
;; https://github.com/osener/github-browse-file
(use-package github-browse-file
  :commands (github-browse-file
             github-browse-file-blame
             github-browse-commit))
;;
;;
;;; git-messenger.el
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :commands (git-messenger:popup-message
             git-messenger:popup-diff)
  :config
  (setq git-messenger:show-detail t))
