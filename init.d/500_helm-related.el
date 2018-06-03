;;; 500_helm-related.el ---                          -*- lexical-binding: t; -*-

;;;
;;; Helm
;; https://emacs-helm.github.io/helm/
(use-package helm
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-z" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ;;
         :map my-key-map
         ("z" . helm-for-files)
         ;; Original M-x
         ("x" . execute-extended-command)
         )
  :config
  ;; Global limit for number of candidates displayed.
  (setq helm-candidate-number-limit 200)
  ;; Always use the other window for helm (below if no other window exists)
  (setq helm-split-window-default-side 'below)
  ;; Emulate `kill-line' in helm minibuffer
  ;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))
  ;;
;;;  helm-config.el
  (use-package helm-config
    :config
    ;; helm-command-prefix
    (setq helm-command-prefix-key "C-x c"))
  ;;
;;;  helm-ring.el
  ;; kill-ring, mark-ring, and register browsers for helm.
  ;; Make helm-mark-ring follow
  ;; https://groups.google.com/forum/#!topic/emacs-helm/US8FWnfRu5o
  (use-package helm-ring
    :hook (helm-before-initialize . (lambda ()
                                      ;; Set follow attribute to 1 (yes) for helm-source-mark-ring
                                      (helm-attrset 'follow 1 helm-source-mark-ring))))
  ;;
;;;  helm-buffers.el
  (use-package helm-buffers
    :config
    ;; Max length of buffer names before truncate.
    (setq helm-buffer-max-length nil))
  ;;
;;;  helm-for-files.el
  (use-package helm-for-files
    :config
    ;; Restrict what to show in helm-for-files
    (setq helm-for-files-preferred-list '(helm-source-buffers-list
                                          helm-source-recentf)))
  ;;
;;;  helm-locate.el
  (use-package helm-locate
    :config
    ;; A list of arguments for locate program.
    ;; https://github.com/emacs-helm/helm/wiki/Locate
    ;; https://github.com/syl20bnr/spacemacs/issues/3280
    (setq helm-locate-command
          (cl-case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es %s")
            ('darwin "mdfind -name %s %s")
            (t "locate %s")))
    ;; Enable fuzzy matching in helm-locate.
    (setq helm-locate-fuzzy-match
          (cl-case system-type
            ('darwin nil)
            (t 't))))
  ;;
;;;  helm-command.el
  (use-package helm-command
    :config
    ;; helm-M-x Save command in extended-command-history even when it fail.
    (setq helm-M-x-always-save-history t))
  ;;
;;;  helm-files.el
  (use-package helm-files
    :config
    ;; Show full path in helm-find-files
    ;; http://emacs.stackexchange.com/questions/22407/view-a-whole-file-name-or-path-in-helm-buffer
    (setq helm-ff-transformer-show-only-basename nil)))
