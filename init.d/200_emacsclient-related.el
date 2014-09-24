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
;;; Handle *helm* buffer not found issue 2014-09-24
;; It is caused by helm--maybe-update-keymap remaining in post-command-hook.
;; helm--maybe-update-keymap requires *helm* to be present.
;; It should be remove-hook'ed, but it does not happen when doing *.Rnw editing.
;; The main problem is emacsclient hits this error and die.
;; Magit uses emacsclient for COMMIT messages, so it does not work.
;;
;; Define a function to remove helm--maybe-update-keymap from post-command-hook
(defun remove-helm--maybe-update-keymap ()
  (remove-hook 'post-command-hook 'helm--maybe-update-keymap))
;;
;; This function should be called before helm--maybe-update-keymap is used.
;;
;; Candidate hooks for making this happen.
;; server-done-hook	Hook run when done editing a buffer for the Emacs server.
;; server-mode-hook	Hook run after entering or leaving `server-mode'.
;; server-switch-hook	Hook run when switching to a buffer for the Emacs server.
;; server-visit-hook	Hook run when visiting a file for the Emacs server.
;;
(add-hook 'server-done-hook	'remove-helm--maybe-update-keymap)
(add-hook 'server-mode-hook	'remove-helm--maybe-update-keymap)
(add-hook 'server-switch-hook	'remove-helm--maybe-update-keymap)
(add-hook 'server-visit-hook	'remove-helm--maybe-update-keymap)
