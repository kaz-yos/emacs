;;; 500_lang-scala.el ---                            -*- lexical-binding: t; -*-

;;;
;;; Setting up environment
;;
;; Getting Started with sbt
;; http://www.scala-sbt.org/0.13/tutorial/index.html
;;
;; Setting up Scala environment (in Japanese)
;; http://futurismo.biz/archives/2449
;;
;; Setting up Emacs, Ensime, SBT for Scala code
;; http://www.viksit.com/tags/scala/setting-up-emacs-ensime-sbt-for-scala-code/
;;
;; emacs + ENSIME for your scala coding
;; https://coderwall.com/p/avlrna


;;;
;;; scala-mode2.el
;; A new scala-mode for emacs24
;; https://github.com/hvesalai/scala-mode2
(require 'scala-mode2)
;;
;; https://github.com/hvesalai/sbt-mode#important-customization-variables-and-other-customizations
(add-hook 'scala-mode-hook '(lambda ()
			      ;; sbt-find-definitions is a command that tries to find (with grep)
			      ;; the definition of the thing at point.
			      (local-set-key (kbd "M-.") 'sbt-find-definitions)
			      ;; use sbt-run-previous-command to re-compile your code after changes
			      (local-set-key (kbd "C-x '") 'sbt-run-previous-command)))


;;;
;;; ensime.el
;; ENhanced Scala Interaction Mode for Emacs
;; https://github.com/ensime/ensime-emacs
;; https://github.com/ensime/ensime-src
;; Manual
;; http://ensime.github.io/ensime-src/index.html
(require 'ensime)
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
;; (require 'sbt-mode)
;; ;;
;; ;; https://github.com/hvesalai/sbt-mode#important-customization-variables-and-other-customizations
;; (add-hook 'sbt-mode-hook '(lambda ()
;; 			    ;; compilation-skip-threshold tells the compilation minor-mode
;; 			    ;; which type of compiler output can be skipped. 1 = skip info
;; 			    ;; 2 = skip info and warnings.
;; 			    (setq compilation-skip-threshold 1)
;; 			    ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
;; 			    ;; cursor to just after prompt.
;; 			    (local-set-key (kbd "C-a") 'comint-bol)
;; 			    ;; Bind M-RET to 'comint-accumulate. This will allow you to add
;; 			    ;; more than one line to scala console prompt before sending it
;; 			    ;; for interpretation. It will keep your command history cleaner.
;; 			    (local-set-key (kbd "M-RET") 'comint-accumulate)))
