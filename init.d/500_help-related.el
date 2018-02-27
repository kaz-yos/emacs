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
;;; helpful.el
;; https://github.com/Wilfred/helpful
;; http://www.wilfred.me.uk/blog/2017/08/30/helpful-adding-contextual-help-to-emacs/
(use-package helpful
  :bind (:map help-map
              ("a" . helpful-at-point)
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("k" . helpful-key))
  :commands (helpful-function
             helpful-command
             helpful-key
             helpful-macro
             helpful-callable
             helpful-variable
             helpful-at-point))
