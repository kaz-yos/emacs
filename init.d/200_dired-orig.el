;;; dired (native dired) configurations
;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
;; % m to choose by regexp; t to toggle selection; M-x find-name-dired wild card search result in dired
;;
;; Human-readable: Show sizes in KB/MB etc
;; http://unix.stackexchange.com/questions/44858/change-view-in-emacs-dired-mode
;; -a show dotfiles -l detail; -h human-readable
(setq dired-listing-switches "-alh")
;;
;; If there are two dired open side by side, copy destination is the other.
(setq dired-dwim-target t)
;; Recursively copy directories.
(setq dired-recursive-copies 'always)
;; C-s matches by file names only.
(setq dired-isearch-filenames t)
;; Automatically revert dired buffer on revisiting.
(setq dired-auto-revert-buffer t)
;; Auto-revert on change. (now auto-revert active globally)
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)


;;; 24.4 issue
;; http://niku.name/articles/2013/04/30/emacsでls%20does%20not%20support%20--dired;%20see%20%60dired-use-ls-dired'%20for%20more%20details.と出たときの対処方法
;; ls does not support --dired; see `dired-use-ls-dired' for more details.
;; This occurs because Mac's ls does not support "--dired" option although there is "-dired"
(setq dired-use-ls-dired 'unspecified)
;;
;; lisp implementation of ls is used in systems without ls
;; (setq ls-lisp-use-insert-directory-program nil)
;; (require 'ls-lisp)


;;; key configuration
(define-key dired-mode-map (kbd "s-d") 'make-directory)


;;; dired+.el
;; This is set for dired+.el in 24.4.
;; Configuration in the dired+.el config file did not work(?)
(setq diredp-hide-details-initially-flag nil)
