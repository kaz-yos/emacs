;;; 500_help-related.el --- -*- lexical-binding: t; -*-

;;;
;;; info
;; Emacs Wiki InfoPath
;; http://www.emacswiki.org/emacs/InfoPath
;; Note: Above the line with ‘File: dir…’ there is a line with a unprintable char, which is showed by emacs as ‘^_’, it is actually one byte (1f in hex), you can input hex-chars with the M-x hexl-mode.
;;
;; 3.5 How do I install a piece of Texinfo documentation?
;; http://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html
;;
;; Setting Up and Using Emacs InfoMode
;; http://pchristensen.com/blog/articles/setting-up-and-using-emacs-infomode/
;;
;; Add path (Need correctly formatted dir file with special format)
(add-to-list 'Info-directory-list (expand-file-name (concat user-emacs-directory
                                                            "info/")))


;;;
;;; Use default eldoc
;; eldoc-extension
;; http://d.hatena.ne.jp/rubikitch/20090207/1233936430
(use-package eldoc-extension
  :config
  (setq eldoc-idle-delay 0.5)
  (setq eldoc-echo-area-use-multiline-p t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-minor-mode-string ""))


;;;
;;; help-fns+.el
;; https://www.emacswiki.org/emacs/help-fns%2B.el
(use-package help-fns+
  :commands (describe-buffer
             describe-command
             describe-option
             describe-key-briefly
             describe-option-of-type
             describe-copying
             describe-file
             describe-keymap
             find-function-on-key))


;;;
;;; helpful.el
;; https://github.com/Wilfred/helpful
(use-package helpful
  ;; This does not work, and breaks other C-S-h bindings
  ;; :bind (("C-S-h f" . helpful-functions))
  :commands (helpful-mode
             helpful-macro
             helpful-update
             helpful-command
             helpful-functions
             helpful-visit-reference))
