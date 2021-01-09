;;; 200_emacsclient-related.el ---                   -*- lexical-binding: t; -*-


;;;
;;; emacsclient
;; http://www.emacswiki.org/emacs/EmacsClient
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
;;
;; Configure as $EDITOR
;; .profile: export EDITOR="emacsclient"
;;
;; 39.3 emacsclient Options
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html
;;
;; Create a new graphical client frame, instead of using an existing Emacs frame.
;; emacsclient --create-frame
;;
;; Create a new client frame on the current text terminal. (Also -t --tty)
;; emacsclient -nw
(defun my-start-emacsclient-c ()
  "Start a process of a new graphical emacsclient."
  (interactive)
  ;; (start-process NAME BUFFER PROGRAM &rest PROGRAM-ARGS)
  (start-process "emacsclient-c" nil
                 "emacsclient" "--create-frame"))
(bind-key "e" 'my-start-emacsclient-c my-key-map)


;;;
;;; server.el
(use-package server
  :config
  ;; Start server unless there is already one running.
  (unless (server-running-p)
    (server-start)))
