;;; 500_dired-related.el ---                         -*- lexical-binding: t; -*-

;;;
;;; Use rsync in dired.el
;; https://github.com/abo-abo/oremacs/blob/2011da0874d27058d3ce8bded5cd8817314c27be/auto.el#L225-L247
;; https://www.reddit.com/r/emacs/comments/58zieq/still_cant_get_over_how_powerful_tramp_is/
(defun ora-dired-rsync (dest)
  (interactive
   (list (expand-file-name
          (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command "rsync -arvz --progress "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))
;; Bind to Y
(define-key dired-mode-map "Y" 'ora-dired-rsync)


;;;
;;; DIRED-HACKS

;;;  dired-subtree.el
;; https://github.com/Fuco1/dired-hacks#dired-subtree
(use-package dired-subtree
  :commands (dired-subtree-insert
             dired-subtree-remove
             dired-subtree-up-dwim)
  ;; https://github.com/jwiegley/use-package#binding-within-local-keymaps
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("<tab>" . dired-subtree-remove)
              ("^" . dired-subtree-up-dwim))
  ;;
  :config
  (defun dired-subtree-up-dwim (&optional arg)
    "Go to parent dir or open parent dir"
    (interactive "p")
    (or (dired-subtree-up arg)
        (dired-up-directory))))


;;;  dired-narrow.el
;; https://github.com/Fuco1/dired-hacks#dired-narrow
;; http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
(use-package dired-narrow
  :commands (dired-narrow
             dired-narrow-regexp
             dired-narrow-fuzzy)
  ;; https://github.com/jwiegley/use-package#binding-within-local-keymaps
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp)))


;;;
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
;; These are used ind dired.
;; M-x runner-add-file
;; M-x runner-add-extension
;; M-x runner-edit-file-at-point
;;
;; These can be called anywhere.
;; M-x runner-add-empty
;; M-x runner-edit
;; M-x runner-delete
(use-package runner
  ;; This has to be available for buffer-do-async-shell-command.
  :demand t
  :commands (runner-add-empty
             runner-edit
             runner-delete)
  :bind (:map dired-mode-map
              ("C-c !" . runner-add-extension))
  ;;
  :config
  (setq runner-init-file (concat user-emacs-directory
                                 "runner-conf.el"))
  (setq runner-show-label t))


;;;  Use runner for current buffer file
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
;;;   Define buffer-do-async-shell-command
(defun buffer-do-async-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND on the current buffer file asynchronously

Modifed version of dired-do-async-shell-command in dired-aux.el
Instead of obtaining file names from dired, gets a file name
from the current buffer."
  (interactive
   ;; Obtain the file name (no path) for the current buffer
   (let ((files `(,(file-name-nondirectory buffer-file-name))))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  ;; Save buffer to avoid executing older code
  (save-buffer)
  ;;
  ;; Kill the ouput buffer. This appears necessary when using over ssh
  (let ((buffer-name-list (mapcar (function buffer-name) (buffer-list)))
        (regexp "*Runner Output*"))
    (when (-filter
           #'(lambda (elt) (string-match regexp elt))
           buffer-name-list)
      (kill-buffer regexp)))
  ;;
  (unless (string-match-p "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (dired-do-shell-command command arg file-list))
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
(bind-key "H-d" 'buffer-do-async-shell-command)
;;
;; One for the regular dired-do-async-shell-command
;; Kill the *Runner Output* buffer before running an async shell command.
;; This is somehow necessary when running over TRAMP.
(defun kill-runner-output (&optional command arg file-list)
  "Kill the *Runner Output* buffer if it exists."
  (interactive)
  (let ((buffer-name-list (mapcar #'buffer-name (buffer-list)))
        (regexp "*Runner Output*"))
    (when (-filter
           #'(lambda (elt) (string-match regexp elt))
           buffer-name-list)
      (kill-buffer regexp))))
;; (advice-add 'dired-do-async-shell-command :before #'kill-runner-output)
;; (advice-remove 'dired-do-async-shell-command #'kill-runner-output)
;;
;;;   Redefine dired-do-async-shell-command
(defun dired-do-async-shell-command (command &optional arg file-list)
  "dired-do-async-shell-command that kills existing *Runner Output*

This is the same as dired-do-async-shell-command except for its
action to kill *Runner Output* before execution."
  ;;
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  ;;
  ;; Kill the ouput buffer. This appears necessary when using over ssh
  (let ((buffer-name-list (mapcar (function buffer-name) (buffer-list)))
        (regexp "*Runner Output*"))
    (when (-filter
           #'(lambda (elt) (string-match regexp elt))
           buffer-name-list)
      (kill-buffer regexp)))
  ;;
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


;;;
;;; Async dired
;; https://github.com/jwiegley/emacs-async
(use-package dired-async
  :demand t
  :config
  (dired-async-mode 1))


;;;
;;; all-the-icons-dired.el
;; https://github.com/domtronn/all-the-icons.el
;; Need to install fonts on the github repo.
;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :commands (all-the-icons-dired-mode)
  :init
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  ;; Face
  (defface all-the-icons-dired-dir-face
    '((((background dark)) :foreground "gray50")
      (((background light)) :foreground "black"))
    "Face for the directory icon"
    :group 'all-the-icons-faces))


;;;
;;; dired-quick-sort.el
;; https://gitlab.com/xuhdev/dired-quick-sort
(use-package dired-quick-sort
  :commands (dired-quick-sort-setup)
  :init
  (add-hook 'after-init-hook 'dired-quick-sort-setup))
