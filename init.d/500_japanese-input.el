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
         ("A-SPC" . skk-mode)
         ("s-j" . skk-mode)
         :map my-key-map
         ("j" . skk-mode))
  ;;
  :config
;;;  Define autoloads necessary when using (package-initialize t) (no activation)
  ;; Taken from the body of `package-initialize'
  ;; Activate the ddskk package only.
  (package-activate 'ddskk)
  ;;
  ;; Config files
  ;; http://openlab.ring.gr.jp/skk/skk-manual-git/She-Ding-huairu.html#g_t_8a2d_5b9a_30d5_30a1_30a4_30eb
  ;; Put everything under this directory.
  (setq skk-user-directory (concat user-emacs-directory
                                   "skk"))
  ;; Initialization file. skk-restart will read this.
  (setq skk-init-file (concat user-emacs-directory
                              "skk/skk"))
  ;; Some files for adaptation
  (setq skk-study-file (concat user-emacs-directory
                               "skk/skk-study"))
  (setq skk-studybackup-file (concat user-emacs-directory
                                     "skk/skk-study.bak"))
  (setq skk-record-file (concat user-emacs-directory
                                "skk/skk-record"))
  ;; Record emacs id of SKK instance that recently accessed skk-jisyo-file.
  (setq skk-emacs-id-file (concat user-emacs-directory
                                  "skk/skk-emacs-id"))
  ;; Dictionaries
  ;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1
  ;; http://openlab.ring.gr.jp/skk/skk-manual-git/Zui-moJi-Ben-De-naShe-Ding-.html#g_t_6700_3082_57fa_672c_7684_306a_8a2d_5b9a
  ;; Use utf-8.
  ;; This affects ALL dictionaries' assumed encoding.
  ;; http://arat.xyz/wordpress/?p=129
  ;; http://openlab.ring.gr.jp/skk/skk/main/etc/dot.skk
  (setq skk-jisyo-code 'utf-8)
  ;;
  (setq skk-kakutei-jisyo nil)
  (setq skk-initial-search-jisyo nil)
  ;;
  ;; Personal dictionary
  (setq skk-jisyo (concat user-emacs-directory
                          "skk/skk-jisyo.utf8"))
  ;; Assume private dictionary file is being access by multiple SKK processes (safer)
  (setq skk-share-private-jisyo t)
  ;; Back up for the personal dictionary
  (setq skk-backup-jisyo (concat user-emacs-directory
                                 "skk/skk-jisyo.utf8.BAK"))
  ;;
  ;; Main dictionary
  ;; Optimized file for faster access (unused by default)
  (setq skk-cdb-large-jisyo nil)
  ;; Non-optimized raw dictionary file
  (let ((aquaskk-dict "~/Library/Application Support/AquaSKK/SKK-JISYO.L")
        (ddskk-dict (concat user-emacs-directory
                            "skk/SKK-JISYO.L")))
    (cond
     ;; Use AquaSKK's dictionary if available.
     ((file-exists-p aquaskk-dict) (setq skk-large-jisyo aquaskk-dict))
     ((file-exists-p ddskk-dict) (setq skk-large-jisyo ddskk-dict))
     (t (setq skk-large-jisyo nil))))
  ;; Fall back dictionary when the dictionary server cannot be reached.
  (setq skk-aux-large-jisyo nil)
  ;;
  ;; Additional dictionaries. Each file must be a sorted dictionary file.
  ;; Specify the encoding if different from the main encoding.
  ;;
  ;; Dictionary prioirity
  ;; http://openlab.ring.gr.jp/skk/skk-manual-git/Ci-Shu-huairunoZhi-Ding-.html
  ;; http://openlab.ring.gr.jp/skk/skk-manual-git/Ci-Shu-Jian-Suo-noShe-Ding-noJu-Ti-Li.html
  (setq skk-search-prog-list
        '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
          (skk-tankan-search 'skk-search-jisyo-file skk-large-jisyo 10000)
          (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
          (skk-search-jisyo-file skk-jisyo 0 t)
          (skk-look)
          (skk-okuri-search)
          (skk-search-cdb-jisyo skk-cdb-large-jisyo)
          ;; Assume euc encoding for the large dictionary.
          ;; Check encoding by nkf --guess SKK-JISYO.L => EUC-JP (LF)
          (skk-search-jisyo-file (cons skk-large-jisyo 'euc-jp) 10000)
          (skk-search-server skk-aux-large-jisyo 10000)
          (skk-search-ja-dic-maybe)
          (skk-search-extra-jisyo-files)
          (skk-search-katakana-maybe)
          (skk-search-sagyo-henkaku-maybe)
          (skk-search-itaiji)))
  ;;
  ;; Configuration
  ;; http://y-mattu.hatenablog.com/entry/2016/09/25/021937
  ;; http://mugijiru.seesaa.net/article/275755984.html
  ;;
  ;; Learning capability
  (use-package skk-study)
  ;; Show hints
  (use-package skk-hint)
  ;; Use : as the hint key
  (setq skk-hint-start-char 58)
  (setq skk-show-annotation t)
  ;;
  ;; Candidates
  ;; Show in minibuffer
  (setq skk-show-candidates-always-pop-to-buffer nil)
  ;; Only one row
  (setq skk-henkan-show-candidates-rows 1)
  ;;
  ;; Dynamic completion (More annoying than useful)
  ;; http://d.hatena.ne.jp/tomoya/20100908/1283945987
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
  (let ((google-ime-skk-file "/usr/local/bin/google-ime-skk"))
    (when (executable-find google-ime-skk-file)
      (setq skk-server-prog google-ime-skk-file)
      ;; Whether to prevent skk server start up from inside emacs
      ;; Manage this service in prodigy.el.
      (setq skk-server-inhibit-startup-server t)
      ;; Host and port
      (setq skk-server-host "localhost")
      (setq skk-server-portnum 55100))))
