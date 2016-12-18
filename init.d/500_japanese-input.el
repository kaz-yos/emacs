;;; 500_japanese-input.el ---                              -*- lexical-binding: t; -*-

;;;
;;; Daredevil SKK (DDSKK)
;; http://openlab.ring.gr.jp/skk/ddskk.html
;;
;; Manuals
;; http://openlab.ring.gr.jp/skk/doc.html
;; http://openlab.ring.gr.jp/skk/skk-manual-git/
;;
;; Dictionaries
;; http://openlab.ring.gr.jp/skk/dic.html
;;
(use-package skk
  :commands (skk-mode)
  :bind (("A-j" . skk-mode)
         ("s-j" . skk-mode))
  ;;
  :config
  ;; Dictionaries
  ;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1
  ;; http://openlab.ring.gr.jp/skk/skk-manual-git/Zui-moJi-Ben-De-naShe-Ding-.html#g_t_6700_3082_57fa_672c_7684_306a_8a2d_5b9a
  (setq skk-large-jisyo     "~/.emacs.d/skk/SKK-JISYO.L")
  (setq skk-cdb-large-jisyo "~/.emacs.d/skk/SKK-JISYO.L.cdb")
  ;;
  ;; Configuration
  ;; http://y-mattu.hatenablog.com/entry/2016/09/25/021937
  ;; Learning capability
  (require 'skk-study)
  ;; Show hints
  (require 'skk-hint)
  ;;
  ;; Assume private dictionary file is being access by
  ;; multiple SKK processes (safer)
  (setq skk-share-private-jisyo t)
  ;;
  ;; Candidates
  ;; Show in minibuffer
  (setq skk-show-candidates-always-pop-to-buffer nil)
  ;; Only one row
  (setq skk-henkan-show-candidates-rows 1)
  ;;
  ;; Dynamic completion (More annoying than useful)
  ;; (setq skk-dcomp-activate t)
  ;; (setq skk-dcomp-multiple-activate t)
  ;; (setq skk-dcomp-multiple-rows 10)
  ;; ;; C-n to select next completion
  ;; (define-key skk-j-mode-map (kbd "C-n") 'skk-completion-wrapper)
  ;;
  ;; Miscellaneous
  ;; RET gives new line if nil
  (setq skk-egg-like-newline nil)
  (setq skk-delete-implies-kakutei nil)
  ;; Use unix look command for English completion
  (setq skk-use-look t)
  ;; Balanced paren isertion
  (setq skk-auto-insert-paren t)
  (setq skk-henkan-strict-okuri-precedence t)
  ;;
  ;;
;;;  Google IME SKK
  ;; http://y-mattu.hatenablog.com/entry/2016/09/25/021937
  ;; Install via $ sudo gem install google-ime-skk
  (when (executable-find "/usr/local/bin/google-ime-skk")
    (setq skk-server-prog "/usr/local/bin/google-ime-skk")
    ;; Allow starting skk server
    (setq skk-server-inhibit-startup-server nil)
    ;; Host and port
    (setq skk-server-host "localhost")
    (setq skk-server-portnum 55100))
  ;;
  ;;
;;;
;;; ac-skk.el
  ;; Not useful if not using auto-complete.el
  ;; https://github.com/myuhe/ac-skk.el
  ;; (require 'ac-skk)
  ;; (ac-skk-enable)
  )
