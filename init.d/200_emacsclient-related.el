;;; 200_emacsclient-related.el ---                   -*- lexical-binding: t; -*-


;;;
;;; emacsclient
;; http://www.emacswiki.org/emacs/EmacsClient
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
;;
;; Configure as $EDITOR
;; .profile: export EDITOR="emacsclient"


;;;
;;; server.el
(use-package server
  :config
  ;; Start server unless there is already one running.
  (unless (server-running-p)
    (server-start)))
