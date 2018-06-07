;;; 500_dired-related.el ---                         -*- lexical-binding: t; -*-


;;; diredfl.el
;; Extra Emacs font lock rules for a more colourful dired
;; https://github.com/purcell/diredfl
(use-package diredfl
  ;; Use in GUI.
  :if (display-graphic-p)
  :hook (dired-mode . diredfl-mode)
  :commands (diredfl-mode))


;;;
;;; dired-rsync.el
;; https://github.com/stsquad/dired-rsync
(use-package dired-rsync
  :commands (dired-rsync)
  :bind (:map dired-mode-map
              ("Y" . dired-rsync)))


;;;
;;; DIRED-HACKS
;; https://github.com/Fuco1/dired-hacks

;;;  dired-subtree.el
;; https://github.com/Fuco1/dired-hacks#dired-subtree
(use-package dired-subtree
  :commands (dired-subtree-insert
             dired-subtree-remove
             dired-subtree-up-dwim)
  ;; https://github.com/jwiegley/use-package#binding-within-local-keymaps
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-remove)
              ("^" . dired-subtree-up-dwim))
  ;;
  :config
  (defun dired-subtree-up-dwim (&optional arg)
    "Go to parent dir or open parent dir"
    (interactive "p")
    (or (dired-subtree-up arg)
        (dired-up-directory)))
  ;;
  ;; No background in terminal.
  (unless (display-graphic-p)
    (set-face-background 'dired-subtree-depth-1-face nil)
    (set-face-background 'dired-subtree-depth-2-face nil)
    (set-face-background 'dired-subtree-depth-3-face nil)
    (set-face-background 'dired-subtree-depth-4-face nil)
    (set-face-background 'dired-subtree-depth-5-face nil)
    (set-face-background 'dired-subtree-depth-6-face nil)))


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
;; https://github.com/thamer/runner
;; 5 Shell Command Guessing
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Shell-Command-Guessing.html
;; The functions `dired-guess-default' (from dired-x.el) and
;; `dired-run-shell-command' (from dired-aux.el) will be redefined.
;;
;; These are used in dired.
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
  ;; Name of file used to save pattern and command database.
  (setq runner-init-file (concat user-emacs-directory
                                 "runner-conf.el"))
  ;; When active hide all output buffers created by `dired-do-shell-command' except when the command string contains `{run:out}'.
  (setq runner-run-in-background t)
  ;;
  ;; Avoid "Text is read-only" when working over tramp.
  (defun dired-run-shell-command-buffer-remove (command)
    "Remove corresponding output buffer before DIRED-RUN-SHELL-COMMAND.

This always kills the output buffer for a fresh start.
A running process is killed. This avoids Text is read-only
issue that happens when running over TRAMP."
    (let (keep-output)
      (while (string-match "{run:out} ?" command)
        (setq command (replace-match "" t t command))
        (setq keep-output t))
      ;;
      (if keep-output
          ;; Limit the buffer name length to 100 to avoid cluttering
          ;; the buffer list
          (let ((outbuf (concat "*Runner Command*: "
                                (if (> (length command) 100)
                                    (concat (substring command 0 100) " ...")
                                  command))))
            ;; If it exists, remove
            ;; https://stackoverflow.com/questions/586735/how-can-i-check-if-a-current-buffer-exists-in-emacs
            (when (get-buffer outbuf)
              (kill-buffer outbuf)))
        ;; If not keeping output
        (when (get-buffer "*Runner Output*")
          (kill-buffer "*Runner Output*")))))
  ;; See help for add-function
  (advice-add 'dired-run-shell-command
              :before #'dired-run-shell-command-buffer-remove)
  ;;
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
    ;; Run
    (unless (string-match-p "&[ \t]*\\'" command)
      (setq command (concat command " &")))
    ;; Functions dired-run-shell-command and dired-shell-stuff-it do the
    ;; actual work and can be redefined for customization.
    ;; dired-run-shell-command is advised above to kill the output buffer.
    (dired-do-shell-command command arg file-list)))


;;;
;;; Async dired
;; https://github.com/jwiegley/emacs-async
(use-package dired-async
  :after dired
  :config
  (dired-async-mode 1))


;;;
;;; dired-quick-sort.el
;; https://gitlab.com/xuhdev/dired-quick-sort
(use-package dired-quick-sort
  :after dired
  ;; ls-lisp-use-insert-directory-program must be non-nil
  :if ls-lisp-use-insert-directory-program
  ;; Automatically use the sorting defined by dired-quick-sort in dired.
  :hook (dired-mode . dired-quick-sort)
  ;; Invoke the dired-quick-sort hydra.
  :bind (:map dired-mode-map
              ("s" . hydra-dired-quick-sort/body)))


;;;
;;; dired-recent.el
;; https://github.com/vifon/dired-recent.el/tree/22104c87593f24ec513dfdf97fc4c8c91defec33
;; Press C-x C-d to select a previously visited directory to open.
(use-package dired-recent
  :after dired
  :config
  (setq dired-recent-directories-file (concat user-emacs-directory
                                              "dired-history"
                                              "_"
                                              (system-name-sans-domain)))
  (dired-recent-mode 1))
