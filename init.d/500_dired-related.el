;;; dired-plus 2014-02-04
;; http://www.emacswiki.org/emacs/DiredPlus
;; http://ergoemacs.org/emacs/emacs_diredplus_mode.html
(require 'dired+)
;;
;;  Hide/Show Details
;;  -----------------
;;  Starting with Emacs 24.4, listing details are hidden by default.
;;  Use `(' anytime to toggle this hiding.  You can use option
;;  `diredp-hide-details-initially-flag' to change the default/initial
;;  state.  See also option `diredp-hide-details-propagate-flag'.
;;
;;  If you have an Emacs version older than 24.4, you can use library
;;  `dired-details+.el' (plus `dired-details.el') to get similar
;;  behavior.
;;
;; Show details by default in 24.4 (does not work here? configure in dired config)
;; (setq diredp-hide-details-initially-flag nil)
;;
;; Other dired inherit the current setting
;; (setq diredp-hide-details-propagate-flag t)
;;


;;; dired-subtree.el
;; https://github.com/Fuco1/dired-hacks/blob/master/dired-subtree.el
(require 'dired-subtree)
;;
;; http://rubikitch.com/2014/12/22/dired-subtree/
;; i is subtree
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
;; tab folding
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-remove)
;; C-x n n for narrowing
(define-key dired-mode-map (kbd "C-x n n") 'dired-subtree-narrow)
;; ^ for dired-subtree
(defun dired-subtree-up-dwim (&optional arg)
  "Go to parent dir or open parent dir"
  (interactive "p")
  (or (dired-subtree-up arg)
      (dired-up-directory)))
(define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)


;;; runner.el
;; Flexible file type specific shell command in dired
;;
;; https://github.com/thamer/runner
;; http://rubikitch.com/2015/01/13/runner-3/
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Shell-Command-Guessing.html
;;
;; The functions `dired-guess-default' (from dired-x.el) and
;; `dired-run-shell-command' (from dired-aux.el) will be redefined.
;;
(require 'runner)
(define-key dired-mode-map (kbd "C-c !") 'runner-add-extension)


;;; Use runner for current buffer file
;;
;; Explanation in dired-x.el
;; GUESS SHELL COMMAND
;; Brief Description:
;;
;; * `dired-do-shell-command' is bound to `!' by dired.el.
;;
;; * `dired-guess-shell-command' provides smarter defaults for
;;    dired-aux.el's `dired-read-shell-command'.
;;
;; * `dired-guess-shell-command' calls `dired-guess-default' with list of
;;    marked files.
;;
;; * Parse `dired-guess-shell-alist-user' and
;;   `dired-guess-shell-alist-default' (in that order) for the first REGEXP
;;   that matches the first file in the file list.
;;
;; * If the REGEXP matches all the entries of the file list then evaluate
;;   COMMAND, which is either a string or a Lisp expression returning a
;;   string.  COMMAND may be a list of commands.
;;
;; * Return this command to `dired-guess-shell-command' which prompts user
;;   with it.  The list of commands is put into the list of default values.
;;   If a command is used successfully then it is stored permanently in
;;   `dired-shell-command-history'.
;;
(defun buffer-do-async-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND on the current buffer file asynchronously

Modifed version of dired-do-async-shell-command in dired-aux.el
Instead of obtaining file names from dired, gets a file name
from the current buffer."
  (interactive
   (let ((files `(,buffer-file-name)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match-p "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (dired-do-shell-command command arg file-list))


;;;
;;; neotree.el
;; https://github.com/jaypei/emacs-neotree
;; https://www.emacswiki.org/emacs/NeoTree
;; http://kiririmode.hatenablog.jp/entry/20150806/1438786800
(use-package neotree
  :commands (neotree)
  :config
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-persist-show t)
  (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  ;; Projectile support
  (setq projectile-switch-project-action 'neotree-projectile-action))



;;;
;;; peep-dired.el
;; https://github.com/asok/peep-dired
;; http://pragmaticemacs.com/emacs/quickly-preview-images-and-other-files-with-peep-dired/
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired))
  :config
  (setq peep-dired-cleanup-eagerly t)
  ;; (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4")))
