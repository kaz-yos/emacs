;;; 500_lang-scala.el ---                            -*- lexical-binding: t; -*-

;; Install scala and sbt
;; brew install scala sbt

;;; Setting up environment
;; Getting Started with sbt
;; http://www.scala-sbt.org/0.13/tutorial/index.html
;; (in Japanese)
;; http://futurismo.biz/archives/2449

;;;
;;; scala-mode2.el
;; A new scala-mode for emacs24
;; https://github.com/hvesalai/scala-mode2
(require 'scala-mode2)


;;;
;;; ensime.el
;; ENhanced Scala Interaction Mode for Emacs
;; https://github.com/ensime/ensime-emacs
;; https://github.com/ensime/ensime-src
;; Manual
;; http://ensime.github.io/ensime-src/index.html
;; (require 'ensime)
;;
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;;
;;; sbt-mode.el
;; https://github.com/hvesalai/sbt-mode
;;
;; M-x sbt-start needs (sbt:find-root) to return a non-nil value
;;
;; Starting from the current default-directory, find the top-most
;; parent directory that is an sbt root. An sbt root directory is
;; identified by the following rules:
;;   - a directory containing a 'project/build.properties' in it.
;;   - a directory that contains a file matching one of the patterns
;;     '*.sbt' or 'project/*.scala' file in it.
;;
(require 'sbt-mode)
