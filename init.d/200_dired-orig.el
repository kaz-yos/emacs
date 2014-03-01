;;; dired (native dired) configuration
;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
;; % m to choose by regexp; t to toggle selection; M-x find-name-dired wild card search result in dired
;;
;; Show sizes in KB/MB etc
;; http://unix.stackexchange.com/questions/44858/change-view-in-emacs-dired-mode
(setq dired-listing-switches "-ahl")
;;
;; If there are two dired open side by side, copy destination is the other.
(setq dired-dwim-target t)
;; Recursively copy directories.
(setq dired-recursive-copies 'always)
;; C-s matches by file names only.
(setq dired-isearch-filenames t)
;; Automatically revert dired buffer on revisiting.
(setq dired-auto-revert-buffer t)
;; Auto-revert on change.
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
