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


;;; Git
;; magit.el
;; e for ediff!
;; Magit User Manual: http://magit.github.io/magit/magit.html
;; emacs wiki magit: http://www.emacswiki.org/emacs/Magit
;; emacs wiki git: http://www.emacswiki.org/emacs/Git
;; http://gom.hatenablog.com/entry/20090524/1243170341
;; Meet Magit on Vimeo: http://vimeo.com/2871241
;; git real basics: http://xahlee.info/linux/git.html
;; magit tutorial: http://ergoemacs.org/emacs/emacs_magit-mode_tutorial.html
;; http://qiita.com/nishikawasasaki/items/f690ee08f6a32d9d03fa
(require 'magit)
;;
;; keybinding for magit-status
(defun my-magit-status (dir)
  (interactive)
  (ess-nuke-trailing-whitespace)
  (save-buffer)
  (magit-status dir))
;;
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "s-g")   'magit-status)
;;
;; change magit diff colors
;; http://readystate4.com/2011/02/22/emacs-changing-magits-default-diff-colors/
;; http://qiita.com/nishikawasasaki/items/f690ee08f6a32d9d03fa
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "SpringGreen3")
     (set-face-background 'magit-diff-add "black")
     (set-face-foreground 'magit-diff-del "firebrick3")
     (set-face-background 'magit-diff-del "black")
     (set-face-foreground 'magit-diff-file-header "blue")
     ;; (set-face-background 'magit-diff-file-header "light yellow") ;Inherit: diff-file-header by default
     ;;(set-face-foreground 'magit-item-highlight "black") ; Inherit: secondary-selection by default
     (set-face-background 'magit-item-highlight "black") ; No highlight
     ))
;;
;; 2014-02-12 Add the --all switch by default to the logginb popup
;; Shown below is how magit-key-mode-popup-* is defined dynamically in magit-key-mode.el
;; (defun magit-key-mode-generate (group)
;;   "Generate the key-group menu for GROUP."
;;   (let ((opts (magit-key-mode-options-for-group group)))
;;     (eval
;;      `(defun ,(intern (concat "magit-key-mode-popup-" (symbol-name group))) nil
;;         ,(concat "Key menu for " (symbol-name group))
;;         (interactive)
;;         (magit-key-mode
;;          (quote ,group)
;;          ;; As a tempory kludge it is okay to do this here.
;;          ,(cl-case group
;;             (logging
;;              '(list "--graph" "--all")) ; 2014-02-12 Can be hacked like this
;;             (diff-options
;;              '(when (local-variable-p 'magit-diff-options)
;;                 magit-diff-options))))))))
;;
;; To see the definition of the function defined here, do the following
;; (symbol-function 'magit-key-mode-popup-logging)
;;
;; These just replace the dynamically created functions with statically made ones.
;;
;; Logging shows all branches by default (--all option added by default)
(defun magit-key-mode-popup-logging ()
  "Key menu for logging (--graph --all by default)"
  (interactive)
  (magit-key-mode 'logging
		  (list "--graph" "--all")))
;;
;; Merging does not use fast-forward by default (--no-ff option added by default)
(defun magit-key-mode-popup-merging ()
  "Key menu for merging (--no-ff by default)"
  (interactive)
  (magit-key-mode 'merging
		  (list "--no-ff")))
;;
;;
;; Configure fringe for git-gutter 2014-02-02
;; http://stackoverflow.com/questions/11373826/how-to-disable-fringe-in-emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fringes.html
(set-fringe-mode '(0 . 1))
;;
;;
;;; git-gutter-fringe+ (fringe version. depends on git-gutter+) 2014-02-02
;; Does not work in .emacs.d (not elisp in general) 2014-03-01
;; https://github.com/nonsequitur/git-gutter-fringe-plus
;; fringe-helper.el is required.
(require 'git-gutter-fringe+)
;; Mute the mode-line
(setq git-gutter+-lighter "")
;; active everywhere
(global-git-gutter+-mode)
;; Show on the right side
(setq git-gutter-fr+-side 'right-fringe)
;;
;; Moving between hunks
(global-set-key (kbd "A-p") 'git-gutter+-previous-hunk)
(global-set-key (kbd "A-n") 'git-gutter+-next-hunk)
;;
;;
;;; git-timemachine.el
;; Use git-timemachine to browse historic versions of a file with p
;; (previous) and n (next).
(require 'git-timemachine)
;;
;;
;;; github-browse-file.el
;; https://github.com/osener/github-browse-file
(require 'github-browse-file)
;;
;;
;;; modeline-git-branch.el
;; http://qiita.com/acple@github/items/3709174ab24c5d82423a
;; https://github.com/acple/modeline-git-branch
(require 'modeline-git-branch)



;;; Mercurial
;; A simple Emacs interface for the Mercurial (Hg) Distributed SCM. 2013-09-09
(require 'ahg)
