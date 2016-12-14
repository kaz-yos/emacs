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
  :bind (("A-j" . skk-mode)
         ("s-j" . skk-mode))
  ;;
  :config
  ;; Dictionaries
  ;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1
  ;; http://openlab.ring.gr.jp/skk/skk-manual-git/Zui-moJi-Ben-De-naShe-Ding-.html#g_t_6700_3082_57fa_672c_7684_306a_8a2d_5b9a
  (setq skk-large-jisyo     "~/.emacs.d/skk/SKK-JISYO.L")
  (setq skk-cdb-large-jisyo "~/.emacs.d/skk/SKK-JISYO.L.cdb")
;;;
;;; ac-skk.el
  ;; https://github.com/myuhe/ac-skk.el
  (require 'ac-skk))
