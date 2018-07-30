;;; 200_dired-orig.el ---                            -*- lexical-binding: t; -*-

;;;
;;; dired (native dired) configurations
(use-package dired
  :demand
  :bind (:map dired-mode-map
              ("s-d" . make-directory))
  :config
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
  (let ((gls (executable-find "gls")))
    (when gls
      (setq insert-directory-program gls)))
  ;;
  ;; lisp implementation of ls is used in systems without ls
  ;; (setq ls-lisp-use-insert-directory-program nil)
  ;; (require 'ls-lisp)
  ;;
  ;; Human-readable: Show sizes in KB/MB etc
  ;; http://unix.stackexchange.com/questions/44858/change-view-in-emacs-dired-mode
  ;; -a show dotfiles; -l detail; -h human-readable
  ;; https://www.safematix.com/system/linux/ls-date-format/
  (setq dired-listing-switches "-alh")
  ;;
  ;; Open read-only using an advice.
  (defun read-only-mode-on ()
    (read-only-mode +1))
  ;; Add to both of these.
  ;; (advice-add 'dired-find-file
  ;;             :after 'read-only-mode-on)
  (advice-add 'dired-find-file-other-window
              :after 'read-only-mode-on)
  ;;
  ;; If there are two dired open side by side, copy destination is the other.
  ;; M-n shows other candidates including the current directory.
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
  )


;;;
;;; dired-x.el
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/index.html#Top
;; Extra Dired functionality
(use-package dired-x
  :after dired
  :commands (dired-omit-mode
             dired-jump)
  :bind (("s-d" . dired-jump)
         :map dired-mode-map
         ("." . dired-omit-mode))
  :config
  ;; Omit files with regexp
  ;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Omitting-Files-in-Dired.html
  (setq dired-omit-files
        ;; Dot files
        (concat dired-omit-files "\\|^\\..+$")))


;;;
;;; dired-aux.el
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :commands (dired-do-async-shell-command)
  :bind (:map dired-mode-map
              ("M-s M-s" . dired-do-async-shell-command)))
