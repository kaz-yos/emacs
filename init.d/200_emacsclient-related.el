;; Use emacsclient
;; ln -s /Applications/Emacs24.3.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient
;; .profile: export EDITOR="emacsclient"
;; http://www.emacswiki.org/emacs/EmacsClient
(require 'server)
(unless (server-running-p)
  (server-start))
(server-start)		; Start server
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))
