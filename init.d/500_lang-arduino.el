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
  :config)
