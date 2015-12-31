;; -*- lexical-binding: t; -*-

;;;
;;; Erlang-related
;;; erlang.el
(use-package erlang-start
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :interpreter ("escript" . erlang-mode))
