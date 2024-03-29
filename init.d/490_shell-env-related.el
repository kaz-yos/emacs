;;; 490_shell-env-related.el ---                     -*- lexical-binding: t; -*-

;;; Password prompt detection
;; https://emacs.stackexchange.com/questions/21116/how-to-prevent-emacs-from-showing-passphrase-in-m-x-shell
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp "\\|Verification code"))


;;;
;;; shell scripts saved with chmod +x
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)


;;;
;;; shell.el
(use-package shell
  :commands (shell)
  :bind (:map my-key-map
         ("s" . shell))
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*shell\\*" . (display-buffer-same-window))))


;;;
;;; sh-script.el
(use-package sh-script
  :bind (:map sh-mode-map
         ("M-s M-s" . buffer-do-async-shell-command)))


;;;
;;; exec-path-from-shell.el to configure correct $PATH for external files
;; https://github.com/purcell/exec-path-from-shell
;; check with (getenv "PATH")
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  ;; https://github.com/purcell/exec-path-from-shell/issues/36
  ;; Do not pass "-i" (interactive shell) to save time.
  ;; bash options
  ;; -i If the -i option is present, the shell is interactive.
  ;; -l Make bash act as if it had been invoked as a login shell (see INVOCATION below).
  (setq exec-path-from-shell-arguments '("-l"))
  ;; Environment variables to copy.
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  ;; Call exec-path-from-shell-copy-envs on each element of exec-path-from-shell-variables.
  (exec-path-from-shell-initialize))


;;;
;;; bash-completion.el
;; https://github.com/szermatt/emacs-bash-completion
(use-package bash-completion
  :ensure t
  :config
  (bash-completion-setup))


;;;
;;; sudo-edit.el
;; https://github.com/nflath/sudo-edit
(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))


;;;
;;; conda.el
;; Emacs helper library (and minor mode) to work with conda environments
;; https://github.com/necaris/conda.el/
(use-package conda
  :disabled t
  :ensure t
  ;; Must come after exec-path-from-shell
  :if (executable-find "conda")
  :config
  (setq conda-anaconda-home "~/miniconda3/")
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation, include:
  (conda-env-autoactivate-mode t))


;;;
;;; SSH support
;;
;; Passphrase issue in OS X. Conflict with gpg-agent
;; How to pass the user-agent ssh key passphrase through to Emacs [i.e. for magit]?
;; https://www.reddit.com/r/emacs/comments/3skh5v/how_to_pass_the_useragent_ssh_key_passphrase/
;; SSH_AUTH_SOCK must be setenv'ed correctly in OS X. However, gpg-agent overrides this.
;; my-set-gpg-agent-info was the offender. Do not use if gpg-agent is not acting as ssh-agent.
;;
;; Code to fix SSH_AUTH_SOCK back to OS X-native
;; It should be something like: /private/tmp/com.apple.launchd.Kvag2TBKeW/Listeners
(when (eq system-type 'darwin)
  (setenv "SSH_AUTH_SOCK"
          (replace-regexp-in-string "\n$" ""
                                    (shell-command-to-string "ls /private/tmp/*launchd*/Listeners"))))
;;
;;
;;;  ssh.el
;; M-x ssh to run remote shell
;; https://github.com/ieure/ssh-el
;; http://www.emacswiki.org/emacs/ShellDirtrackByPrompt
;;
;; Deleted. Kept for historical record.
(use-package ssh
  :disabled t
  :commands (ssh)
  :config
  (setq ssh-directory-tracking-mode 'ftp)
  ;;
  (add-hook 'ssh-mode-hook
            (lambda ()
              (shell-dirtrack-mode t)
              (setq dirtrackp nil))))


;;;
;;; TRAMP (Transparent Remote Access, Multiple Protocols)
;; Access remote files like local files
;; http://www.emacswiki.org/emacs/TrampMode
;; ad-hoc multi-hop syntax
;; https://www.emacswiki.org/emacs/TrampMode#toc13
;; 5 Configuring TRAMP
;; http://www.gnu.org/software/tramp/#Configuration
;; 6.3 Declaring multiple hops in the file name
;; http://www.gnu.org/software/tramp/#Ad_002dhoc-multi_002dhops
;;;  tramp.el
(use-package tramp
  :config
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Traces-and-Profiles.html
  (setq tramp-verbose 4)
  ;;
  ;; Default method to use for transferring files.
  ;; http://emacs.stackexchange.com/questions/13797/tramp-dired-transfers-files-inline-over-ssh-instead-of-using-scp-externaly
  ;; ssh is faster, but experience frequent invalid base64 data error
  (setq tramp-default-method "ssh")
  ;; scp is slower, but is safer.
  ;; (setq tramp-default-method "scp")
  ;; To respect the default method in tramp-default-method, use /host:/path/to/file.
  ;; This will automatically converted to /default-method:host:/path/to/file.
  ;;
  ;; Respect locally set path
  ;; http://emacs.stackexchange.com/questions/7673/how-do-i-make-trampeshell-use-my-environment-customized-in-the-remote-bash-p
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;
  ;; http://emacs.stackexchange.com/questions/22304/invalid-base64-data-error-when-using-tramp
  ;; The maximum file size where inline copying is preferred over an out-of-the-band copy.
  ;; If it is nil, out-of-the-band copy will be used without a check.
  (setq tramp-copy-size-limit nil)
  ;; The minimum size of compressing where inline transfer.
  (setq tramp-inline-compress-start-size nil)
  ;;
  ;; Handle Odyssey's two-step authentication. Password:, then, Verification code:   2014-01-18
  ;; http://emacs.1067599.n5.nabble.com/emacs-hangs-when-connecting-from-windows-to-linux-with-tcsh-shell-td244075.html
  ;; (setq tramp-password-prompt-regexp "^.*\\([pP]assword\\|[pP]assphrase\\).*: ? *")	; Original
  (setq tramp-password-prompt-regexp "^.*\\([pP]assword\\|[pP]assphrase\\|Verification code\\|Passcode or option\\).*: ? *")
  ;;
  ;; Completion works in the eshell open from within a tramp connection
  ;; http://stackoverflow.com/questions/1346688/ssh-through-emacs-shell
  ;;
  ;; /sudo:hostname for sudo'ing after remote login
  ;; http://qiita.com/miyakou1982/items/d05e1ce07ad632c94720
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
               '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               '((regexp-quote (system-name)) nil nil))
  ;;
  ;; Disable version control to avoid delays.
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  ;; Regexp matching directory names that are not under VC's control.
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;;
  ;; Sudoing current file (need improvement for remote use)
  ;; https://www.reddit.com/r/emacs/comments/58zieq/still_cant_get_over_how_powerful_tramp_is/
  ;; (defun find-file-sudo ()
  ;;   "Reopen the current file as root, preserving point position."
  ;;   (interactive)
  ;;   (let ((p (point)))
  ;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
  ;;     (goto-char p)))
  )


;;;  tramp-term.el
;; https://github.com/randymorris/tramp-term.el
(use-package tramp-term
  :ensure t
  :commands (tramp-term))


;;;
;;; Open in Terminal app
;; http://truongtx.me/2013/09/13/emacs-dired-new-terminal-window-at-current-directory-on-macos/
;; default terminal application path
(defvar osx-term-app-path
  "/Applications/Utilities/Terminal.app"
  "The path to the terminal application to use in open-in-osx-term-app")
;;
;; function to open new terminal window at current directory
(defun open-in-osx-term-app ()
  "Open current directory in dired mode in terminal application.
For OS X only"
  (interactive)
  (shell-command (concat "open -a "
                         (shell-quote-argument osx-term-app-path)
                         " "
                         (shell-quote-argument (file-truename default-directory)))))


;;;
;;; Gnu Privacy Guard (gpg)-related
;;
;;;  gpg-agent handling
;; http://whatthefuck.computer/blog/2015/05/20/re-agent/
;; (defun my-set-gpg-agent-info ()
;;   "Load your gpg-agent.env file in to the environment

;; SSH_AUTH_SOCK and SSH_AGENT_PID are updated for gpg-agent.
;; This is extra useful if you use gpg-agent with --enable-ssh-support
;; Do not use if gpg-agent is not acting as ssh-agent. It will break
;; OS X-native SSH_AUTH_SOCK."
;;   (interactive)
;;   (let ((home (getenv "HOME"))
;;         (old-buffer (current-buffer)))
;;     (with-temp-buffer
;;       (insert-file-contents (concat home "/.gpg-agent-info"))
;;       (goto-char (point-min))
;;       (setq case-replace nil)
;;       (replace-regexp "\\(.*\\)=\\(.*\\)" "(setenv \"\\1\" \"\\2\")")
;;       (eval-buffer)))
;;   (getenv "GPG_AGENT_INFO"))
;;
;; Incompatible with current OS X-naitive SSH_AUTH_SOCK
;; ;; Retrieve and set GPG_AGENT_INFO
;; (my-set-gpg-agent-info)
;; ;; Repeat when idle for 60 sec
;; (run-with-idle-timer 60 t 'my-set-gpg-agent-info)


;;;
;;; SLURM-RELATED
;; Not available on MELPA.
;; https://github.com/ffevotte/slurm.el
;;;  slurm-script-mode.el for scripting
;; https://github.com/ffevotte/slurm.el#slurm-script-mode
(use-package slurm-script-mode
  :commands (slurm-script-mode))
;;
;;;  slurm-mode.el for interaction
;; https://github.com/ffevotte/slurm.el#slurm-mode
(use-package slurm-mode
  ;; https://github.com/jwiegley/use-package/issues/500
  :load-path "~/Dropbox/documents/programming/emacs-lisp-repos/slurm.el/"
  :commands (slurm)
  :config
  ;; If non-nil, the jobs list is filtered by user at start.
  (setq slurm-filter-user-at-start t)
  )


;;;
;;; emamux.el
;; https://github.com/syohex/emacs-emamux
(use-package emamux
  :ensure t
  :commands (emamux:send-command)
  :config
  )


;;;
;;; crontab-e
;;; https://emacs.stackexchange.com/questions/10077/how-to-edit-crontab-directly-within-emacs-when-i-already-have-emacs-open
;;
;; # .---------------- minute (0 - 59)
;; # |  .------------- hour (0 - 23)
;; # |  |  .---------- day of month (1 - 31)
;; # |  |  |  .------- month (1 - 12) OR jan,feb,mar,apr ...
;; # |  |  |  |  .---- day of week (0 - 6) (Sunday=0 or 7)  OR sun,mon,tue,wed,thu,fri,sat
;; # |  |  |  |  |
;; # *  *  *  *  *  command to be executed
;; # *  *  *  *  *  command --arg2 --arg2 file1 file2 2>&2
(defun crontab-e ()
  "Run `crontab -e' in a emacs buffer."
  (interactive)
  ;; (with-editor-async-shell-command COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)
  ;; `async-shell-command' is fine if $EDITOR is correctly set up with run-emacsclient-cli.
  ;; https://alexn.org/wiki/emacs.html#script-for-running-emacsclient
  (async-shell-command "crontab -e"))


;;;
;;; crontab-mode.el
;; https://github.com/emacs-pe/crontab-mode/blob/master/crontab-mode.el
(use-package crontab-mode
  :ensure t
  :mode "/crontab\\.X*[[:alnum:]]+\\'")


;;;
;;; Remove paste bracketing
(defun my-remove-paste-bracketing ()
  (interactive)
  (search-backward "201~")
  (delete-char 4)
  (search-backward "200~")
  (delete-char 4))


;;;
;;; xclip.el
;; http://elpa.gnu.org/packages/xclip.html
;; Use macOS: `pbpaste/pbcopy'.
(use-package xclip
  :ensure t
  :if (and
       ;; Not GUI emacs
       (not (display-graphic-p))
       ;; Additionally one of:
       (or
        ;; macOS has buildint pbpaste/pbcopy.
        (string-equal system-type "darwin")
        ;; Linux requires xclip installed.
        (and (string-equal system-type "gnu/linux")
             (executable-find "xclip"))))
  :config
  (xclip-mode +1))


;;;
;;; clipetty.el
;; Manipulate the system (clip)board with (e)macs from a (tty)
;; https://github.com/spudlyo/clipetty
(use-package clipetty
  :ensure t
  :if (not (display-graphic-p))
  :commands (clipetty-kill-ring-save))


;;;
;;; vterm.el
;; https://github.com/akermu/emacs-libvterm
;; implements a terminal via libvterm
;;
;; Dependencies on macOS
;; $ brew install libvterm
;; $ brew install cmake
;;
;; Compilation on 2020-04-29
;; -- The C compiler identification is AppleClang 11.0.0.11000033
;; -- Check for working C compiler: /Library/Developer/CommandLineTools/usr/bin/cc
;; -- Check for working C compiler: /Library/Developer/CommandLineTools/usr/bin/cc - works
;; -- Detecting C compiler ABI info
;; -- Detecting C compiler ABI info - done
;; -- Detecting C compile features
;; -- Detecting C compile features - done
;; -- No build type selected, defaulting to RelWithDebInfo
;; -- Configuring done
;; -- Generating done
;; -- Build files have been written to: /Users/kazuki/.emacs.d/elpa/vterm-20200429.428/build
;; Scanning dependencies of target libvterm
;; OMITTED
(use-package vterm
  :ensure t
  :commands (vterm
             vterm-other-window))


;;;
;;; multi-vterm.el
;; https://github.com/suonlight/multi-vterm/
;; Dropped .elc to avoid:
;; Autoloading file .../multi-vterm.elc failed to define function multi-vterm
(use-package multi-vterm
  :ensure t
  :commands (multi-vterm
             multi-vterm-next
             multi-vterm-prev
             multi-vterm-dedicated-toggle
             multi-vterm-projectile)
  :config
  (setq multi-vterm-program nil)
  (setq multi-vterm-buffer-name "vterminal")
  (setq multi-vterm-dedicated-window-height 30))
