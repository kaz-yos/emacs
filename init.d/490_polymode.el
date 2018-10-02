;;;
;;; polymode.el
;; Polymode is a framework for multiple major modes (MMM) inside a single Emacs buffer.
;; https://polymode.github.io
;; https://github.com/vitoshka/polymode
;; https://github.com/vspinu/polymode/blob/master/readme.md#basic-usage

;;;  MARKDOWN-RELATED
(use-package poly-markdown
  :mode ("\\.md" . poly-markdown-mode))
