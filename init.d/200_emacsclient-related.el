;;; Use emacsclient
;;
;; emacsclient is a symlink to the one in Application
;; ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient
;;
;; Configure as $EDITOR
;; .profile: export EDITOR="emacsclient"
;;
;; http://www.emacswiki.org/emacs/EmacsClient
;; http://qiita.com/syohex/items/2bdcd9cd5b701b1112e6
(require 'server)
;;
;;;
;;; Start server unless there is already one running.
(unless (server-running-p)
  (server-start))
;;
(defvar server-buffer-clients)
;;
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))
;;
;;
;;;
;;; Handle *helm* buffer not found issue 2014-09-24, 2015-07-01
;;
;; Following errors were seen in *Messages* after opening *.Rnw file
;;
;; Error in post-command-hook (helm--update-header-line): (error "No buffer named *helm*")
;; Error in post-command-hook (helm--maybe-update-keymap): (error "No buffer named *helm*")
;; Error in post-command-hook (helm--update-header-line): (error "No buffer named *helm*")
;; Error in post-command-hook (helm--maybe-update-keymap): (error "No buffer named *helm*")
;;
;; These were caused by helm functions unnecessarily remaining in post-command-hook.
;; These functions requires *helm* buffer to be present.
;; They should be remove-hook'ed, but it does not happen when doing *.Rnw editing.
;; The main problem is emacsclient hits this error and die.
;; Magit uses emacsclient for COMMIT messages, so it does not work.
;;
;; helm-internal function is supposed to add-hook and remove-hook these
;; Buffer: File (grep): ~/.emacs.d/elpa/helm-20150630.1150/helm.el
;; 1989                  (add-hook 'post-command-hook 'helm--maybe-update-keymap)
;; 1990                  (add-hook 'post-command-hook 'helm--update-header-line)
;; 2004       (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
;; 2005       (remove-hook 'post-command-hook 'helm--update-header-line)
;; 2144           (unless (cl-loop for h in post-command-hook
;; 2147             (add-hook 'post-command-hook 'helm--maybe-update-keymap)
;; 2148             (add-hook 'post-command-hook 'helm--update-header-line))
;; 3421 ;; Now we run this in post-command-hook, it is
;; 3879   ;; This should be used in `post-command-hook',
;;
;; Define a function to remove helm functions from post-command-hook
(defun remove-helm-functions ()
  (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
  ;; 2015-07-01 The following function was also remaining in the hook.
  (remove-hook 'post-command-hook 'helm--update-header-line))
;;
;; 2015-07-01
;; This function itself is not remaining in the post-command-hook?
;;
;; Candidate hooks for making this happen.
;; server-done-hook	Hook run when done editing a buffer for the Emacs server.
;; server-mode-hook	Hook run after entering or leaving `server-mode'.
;; server-switch-hook	Hook run when switching to a buffer for the Emacs server.
;; server-visit-hook	Hook run when visiting a file for the Emacs server.
;;
;; (add-hook 'server-done-hook   'remove-helm--maybe-update-keymap)
;; (add-hook 'server-mode-hook   'remove-helm--maybe-update-keymap)
;; (add-hook 'server-switch-hook 'remove-helm--maybe-update-keymap)
;; (add-hook 'server-visit-hook  'remove-helm--maybe-update-keymap)
;;
;; This hacky universal solution works.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Overview.html#Command-Overview
;; (add-hook 'post-command-hook 'remove-helm-functions)
;; 2015-07-01 Changed to the following.
(add-hook 'pre-command-hook 'remove-helm-functions)
