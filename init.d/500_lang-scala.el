;;; 500_lang-scala.el ---                            -*- lexical-binding: t; -*-


;;;
;;; scala-mode2
;; A new scala-mode for emacs24
;; https://github.com/hvesalai/scala-mode2
(require 'scala-mode2)


;;;
;;; ensime
;; ENhanced Scala Interaction Mode for Emacs
;; https://github.com/ensime/ensime-src
;; Manual
;; http://ensime.github.io/ensime-src/index.html
(require 'ensime)
;;
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
