;;; Helm
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688		; arabiki
;; http://d.hatena.ne.jp/a_bicky/20140125/1390647299		; arabiki 2
;; http://sleepboy-zzz.blogspot.com/2012/09/anythinghelm.html	; in general
;; http://d.hatena.ne.jp/syohex/20121207/1354885367		; some useful tips
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline12
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (use-package helm-config)
  :bind (("M-x" . helm-M-x)
         ("s-x" . helm-M-x)
         ;; Escape hatches
         ("A-x" . execute-extended-command)
         ("A-M-x" . execute-extended-command)
         ("C-M-x" . execute-extended-command)
         ("s-e" . execute-extended-command)
         ("H-x" . execute-extended-command)
         ;;
         ("C-z" . helm-for-files)
         ("C-c z" . helm-for-files)
         ;; This does not support jump to package on a package name
         ;; ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ;; ("C-x b" . helm-buffers-list)
         ("C-x C-r" . helm-recentf)
         ;; ("C-^" . helm-c-apropos)
         ;; This brings back the last helm used
         ;; ("C-;" . helm-resume)
         ;; ("s-c" . helm-occur)
         ("A-m" . helm-mark-ring)
         ("A-l" . helm-locate)
         ;; ("M-z" . helm-do-grep)
         ;; ("C-S-h" . helm-descbinds)
         ;;
         :map my-key-map
         ("z" . helm-for-files)
         )
  :config
  ;; Make helm-mark-ring follow
  ;; https://groups.google.com/forum/#!topic/emacs-helm/US8FWnfRu5o
  (use-package helm-ring
    :config
    (add-hook 'helm-before-initialize-hook
              #'(lambda ()
                  ;; Set follow attribute to 1 (yes) for helm-source-mark-ring
                  (helm-attrset 'follow 1 helm-source-mark-ring)
                  ;; (helm-attrset 'follow 1 helm-source-mark-ring)
                  )))
  ;;
  ;; Restrict what to show in helm-for-files
  (setq helm-for-files-preferred-list '(helm-source-buffers-list
                                        helm-source-recentf))
  ;;
  ;; locate command
  ;; https://github.com/emacs-helm/helm/wiki/Locate
  ;; https://github.com/syl20bnr/spacemacs/issues/3280
  (setq helm-locate-command
        (case system-type
          ('gnu/linux "locate -i -r %s")
          ('berkeley-unix "locate -i %s")
          ('windows-nt "es %s")
          ('darwin "mdfind -name %s %s")
          (t "locate %s")))
  (setq helm-locate-fuzzy-match
        (case system-type
          ('darwin nil)
          (t 't)))
  ;;
  (setq helm-idle-delay 0.3)
  (setq helm-input-idle-delay 0.3)
  (setq helm-candidate-number-limit 200)
  (setq helm-M-x-always-save-history t)
  ;; Max length of buffer names before truncate.
  (setq helm-buffer-max-length nil)
  ;; Show full path in helm-find-files
  ;; http://emacs.stackexchange.com/questions/22407/view-a-whole-file-name-or-path-in-helm-buffer
  (setq helm-ff-transformer-show-only-basename nil)
  ;; This causes cycle with Buffers above Recentf and is useless.
  ;; (setq helm-move-to-line-cycle-in-source t)
  ;; Always use the other window for helm (below if no other window exists)
  (setq helm-split-window-default-side 'other)
  ;; helm for isearch 2014-02-01
  ;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  ;;
  ;; Emulate `kill-line' in helm minibuffer
  ;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end)))))
;;
;;
;;; debug
;; (defvar *helm for files*) in helm.el
;; 2014-09-21 Add the following to test how auctex works with this
;; (setq helm-buffer "*helm for files*")



;;;
;;; Optional helm packages
