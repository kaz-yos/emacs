;;; dired-plus 2014-02-04
;; http://www.emacswiki.org/emacs/DiredPlus
;; http://ergoemacs.org/emacs/emacs_diredplus_mode.html
(use-package dired+
  :demand t)
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
;;; dired-subtree.el
;; https://github.com/Fuco1/dired-hacks/blob/master/dired-subtree.el
(use-package dired-subtree
  :config
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
  (define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim))


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
(use-package runner
  :config
  (define-key dired-mode-map (kbd "C-c !") 'runner-add-extension))


;;;
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
