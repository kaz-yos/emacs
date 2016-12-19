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
(add-to-list 'Info-directory-list (expand-file-name "~/.emacs.d/info/"))


;;;
;;; Use default eldoc (loaded automatically)
;; (require 'eldoc)
;; eldoc-extension
;; http://d.hatena.ne.jp/rubikitch/20090207/1233936430
(use-package eldoc-extension
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-minor-mode-string ""))


;;;
;;; help-fns+.el
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


;; ;; guide-key.el	for prefix key live help
;; ;; ELPA 2014-02-03
;; ;; See elpa help file. It is very helpful.
;; ;; https://github.com/kbkbkbkb1/guide-key
;; ;; http://www.kaichan.info/blog/2013-12-22-emacs-advent-calendar-2013-22.html
;; ;; http://www.kaichan.info/blog/2012-12-03-emacs-advent-calendar-2012-03.html
;; (require 'guide-key)
;; ;;
;; ;; Redefine to erase the lighter (2014-02-06 suggetion by author)
;; (setcdr (assq 'guide-key-mode minor-mode-alist) (list ""))
;; ;; Brute-force method by redefining the minor mode definition.
;; ;; (define-minor-mode guide-key-mode
;; ;;   "Toggle guide key mode.
;; ;; In guide key mode, Guide following keys to an input key sequence
;; ;; automatically and dynamically.
;; ;; With a prefix argument ARG, enable guide key mode if ARG is
;; ;; positive, otherwise disable."
;; ;;   :global t
;; ;;   :lighter ""
;; ;;   (funcall (if guide-key-mode
;; ;;                'guide-key/turn-on-timer
;; ;;              'guide-key/turn-off-timer)))
;; ;;
;; ;; Guide everything
;; (setq guide-key/guide-key-sequence '("C-x"))
;; (setq guide-key/recursive-key-sequence-flag t)
;; ;; Configure key sequences to guide
;; ;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v"))
;; ;; Specification by families
;; ;; (setq guide-key/highlight-command-regexp "rectangle\\|register")
;; ;;
;; ;; Keychord integration
;; ;; (guide-key/key-chord-hack-on)
;; ;; (setq guide-key/guide-key-sequence '("<key-chord> : h" "<key-chord> h :"))
;; ;;
;; ;; Set delay
;; (setq guide-key/idle-delay 0.7)
;; ;; Set font size (negative for smaller)
;; (setq guide-key/text-scale-amount 0.1)
;; ;; Show at the bottom
;; ;; http://shibayu36.hatenablog.com/entry/2013/08/05/214023
;; (setq guide-key/popup-window-position 'bottom)
;; ;; Activate
;; (guide-key-mode 1)  ; guide-key-mode on
