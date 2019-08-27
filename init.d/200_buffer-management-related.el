;;; Buffere-related configurations  -*- lexical-binding: t; -*-


;;;
;;; Unique buffer names
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;;
;;; autorevert.el
(use-package autorevert
  :commands (auto-revert-mode)
  :bind (:map my-key-map
              ("a" . auto-revert-mode))
  ;;
  :config
  ;; Active in all buffers
  (setq global-auto-revert-mode nil)
  ;; Even in non-file buffers
  (setq global-auto-revert-non-file-buffers t)
  ;; VC status change is also captured
  (setq auto-revert-check-vc-info t)
  ;; No ARev in mode-line
  ;; (setq auto-revert-mode-text "")
  ;; http://pragmaticemacs.com/emacs/make-emacs-a-bit-quieter/
  ;; When nil, Auto-Revert Mode does not generate any messages.
  (setq auto-revert-verbose nil)
  )


;;;
;;; Kill process buffer without confirmation?
;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
(setq kill-buffer-query-functions
      ;; Delete members of LIST which are eq to ELT, and return the result.
      (delq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
