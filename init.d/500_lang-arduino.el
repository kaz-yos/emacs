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
  :bind (;;
         :map arduino-mode-map
         ("M-s M-s" . my-arduino-compile-and-upload))
  ;;
  :config
  ;;
  (defun my-arduino-compile ()
    (interactive)
    (compile "arduino-cli compile --fqbn arduino:avr:uno --build-path build"))
  ;;
  (defun my-arduino-detect-port (&optional fqbn)
    "Detect Arduino serial port using arduino-cli JSON output.
If FQBN is non-nil, prefer a matching board."
    (let* ((json-output
            (shell-command-to-string
             "arduino-cli board list --format json"))
           (data (json-parse-string json-output :object-type 'alist))
           (detected (alist-get 'detected_ports data)))
      ;;
      (unless detected
        (error "No serial ports detected by arduino-cli"))
      ;; Prefer a port whose matching_boards contains the requested FQBN
      (or
       (when fqbn
         (let ((match
                (seq-find
                 (lambda (entry)
                   (seq-some
                    (lambda (b)
                      (string= fqbn (alist-get 'fqbn b)))
                    (alist-get 'matching_boards entry)))
                 detected)))
           (when match
             (alist-get 'address (alist-get 'port match)))))
       ;; Otherwise, fall back to first port that has matching_boards
       (let ((fallback
              (seq-find
               (lambda (entry)
                 (alist-get 'matching_boards entry))
               detected)))
         (when fallback
           (alist-get 'address (alist-get 'port fallback))))

       ;; Absolute last resort: first detected port
       (alist-get 'address (alist-get 'port (car detected))))))
  ;;
  (defun my-arduino-upload ()
    (interactive)
    (let* ((fqbn "arduino:avr:uno")
           (port (my-arduino-detect-port fqbn)))
      (compile
       (format
        "arduino-cli upload -p %s --fqbn %s --build-path build"
        port fqbn))))
  ;;
  (defun my-arduino-compile-and-upload ()
    (interactive)
    (let* ((fqbn "arduino:avr:uno")
           (port (my-arduino-detect-port fqbn)))
      (compile
       (format
        "arduino-cli compile --fqbn %s --build-path build && \
arduino-cli upload -p %s --fqbn %s --build-path build"
        fqbn port fqbn)))))
