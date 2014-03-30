;;; Use emacsclient
;;
;; emacsclient is a symlink
;; ln -s /Applications/Emacs24.3.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient
;;
;; configured as $EDITOR
;; .profile: export EDITOR="emacsclient"
;;;
;; http://www.emacswiki.org/emacs/EmacsClient
;; http://qiita.com/syohex/items/2bdcd9cd5b701b1112e6
(require 'server)
;; Start server unless there is already one running.
(unless (server-running-p)
  (server-start))
;;
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))
