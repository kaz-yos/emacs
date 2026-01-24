;;; 500_lang-arduino.el ---                          -*- lexical-binding: t; -*-


;;;
;;; arduino-mode.el
;; arguino-cli
;; https://docs.arduino.cc/arduino-cli/
;; $ brew install arduino-cli
;; $ arduino-cli config init
;; $ arduino-cli core update-index
;; $ arduino-cli core install arduino:avr
;; $ arduino-cli compile --fqbn arduino:avr:uno --only-compilation-database
(use-package arduino-mode
  :ensure t
  :mode "\\.ino\\'"
  :config
  ;;
  (defun my-arduino-compile ()
    (interactive)
    (compile "arduino-cli compile --fqbn arduino:avr:uno"))
  ;;
  (defun my-arduino-upload ()
    (interactive)
    (compile "arduino-cli upload -p /dev/tty.usbmodem14301 --fqbn arduino:avr:uno")))
