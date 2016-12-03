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
;; Auto-revert on change.
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
;; Allow file permission rewrite
;; (setq wdired-allow-to-change-permissions t)
;;
;;; key configuration
(add-hook 'dired-mode-hook
          '(lambda () (define-key dired-mode-map (kbd "s-d") 'make-directory)))
;; Opening parent folder of current buffer
(global-set-key (kbd "s-d") #'(lambda ()
                                (interactive)
                                (find-file "./")))

;;; File type specific shell command
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Shell-Command-Guessing.html
;; Use runner.el
;; https://github.com/thamer/runner
;; http://rubikitch.com/2015/01/13/runner/


;;; ls configuration
;; http://niku.name/articles/2013/04/30/emacsでls%20does%20not%20support%20--dired;%20see%20%60dired-use-ls-dired'%20for%20more%20details.と出たときの対処方法
;; ls does not support --dired; see `dired-use-ls-dired' for more details.
;; This occurs because Mac's ls does not support "--dired" option although there is "-dired"
;; To leave it unspecified, do this.
;; (setq dired-use-ls-dired 'unspecified)
;;
;; Use gls in Homebrew coreutils instead to allow use of --dired
;; http://www.topbug.net/blog/2013/04/14/install-and-use-gnu-command-line-tools-in-mac-os-x/
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2012-07/msg01208.html
;; http://qiita.com/maangie/items/5a80ae50c13d14368a72
(setq ls-lisp-use-insert-directory-program t)
;; Check existance of gls and use it if it exists
(let ((gls "/usr/local/bin/gls"))
  (when (file-exists-p gls)
      (setq insert-directory-program gls)))
;;
;; lisp implementation of ls is used in systems without ls
;; (setq ls-lisp-use-insert-directory-program nil)
;; (require 'ls-lisp)


;;; dired+.el
;; This is set for dired+.el in 24.4.
;; Configuration in the dired+.el config file did not work(?)
(setq diredp-hide-details-initially-flag nil)
