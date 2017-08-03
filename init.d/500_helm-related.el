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
  :bind (("M-x" .     helm-M-x)
         ;; ("C-M-x" .   execute-extended-command)
         ("C-z" .     helm-for-files)
         ;; This does not support jump to package on a package name
         ;; ("C-x C-f" . helm-find-files)
         ("M-y" .     helm-show-kill-ring)
         ;; ("C-x b" .   helm-buffers-list)
         ("C-x C-r" . helm-recentf)
         ;; ("C-^" .     helm-c-apropos)
         ;; This brings back the last helm used
         ;; ("C-;" .     helm-resume)
         ;; ("s-c" .     helm-occur)
         ("A-m" .     helm-mark-ring)
         ("A-l" .     helm-locate)
         ;; ("M-z" .     helm-do-grep)
         ;; ("C-S-h" .   helm-descbinds)
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
  (setq helm-for-files-preferred-list '(helm-source-buffers-list helm-source-recentf))
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
;;
;;;  helm-descbinds.el
;; Replace describe-bindings with helm interface
;; http://emacs-jp.github.io/packages/helm/helm-descbinds.html
;; http://d.hatena.ne.jp/buzztaiki/20081115/1226760184 (anything version)
(use-package helm-descbinds
  :commands (helm-descbinds)
  :config
  (helm-descbinds-mode))


;;;  helm-migemo.el
;; https://github.com/emacs-jp/helm-migemo
;; http://sleepboy-zzz.blogspot.com/2013/02/helm-migemo.html
(use-package helm-migemo
  :disabled t
  ;; Mac-only
  :if (eq system-type 'darwin))


;;;  wgrep-helm.el
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep-helm
  :commands (wgrep-helm-setup)
  :init
  (add-hook 'helm-grep-mode-hook 'wgrep-helm-setup)
  (add-hook 'helm-moccur-mode-hook 'wgrep-helm-setup))


;;;  helm-ag.el
;; https://github.com/syohex/emacs-helm-ag
;; http://qiita.com/l3msh0@github/items/97909d6e2c92af3acc00
(use-package helm-ag
  :commands (helm-ag
             helm-ag-this-file)
  :config
  ;; Keymap
  ;; https://github.com/syohex/emacs-helm-ag#keymap
  ;; No grouping makes more sense with narrowing by helm.
  (setq helm-ag-base-command "ag --smart-case --nogroup")
  ;; -t --all-text: Search all text files. This doesn't include hidden files.
  (setq helm-ag-command-option "--all-text")
  ;; Save buffers you edit at completed. (C-x C-q to edit. C-c C-c to complete.)
  (setq helm-ag-edit-save t)
  (setq helm-ag-thing-at-point 'symbol))



;;;  helm-open-github
;;  2014-02-01 OAutho required
;; http://shibayu36.hatenablog.com/entry/2013/01/18/211428
(use-package helm-open-github
  :commands (helm-open-github-from-file
             helm-open-github-from-commit
             helm-open-github-from-issues))


;;;  helm-mode-manager.el
;; Select and toggle major and minor modes with helm
;; https://github.com/istib/helm-mode-manager
(use-package helm-mode-manager
  :commands (helm-switch-major-mode
             helm-enable-minor-mode
             helm-disable-minor-mode))


;;;  helm-dash.el
;; http://fukuyama.co/helm-dash
;; http://kapeli.com/dash
(use-package helm-dash
  :commands (helm-dash
             helm-dash-at-point))


;;;  helm-swoop.el
(use-package helm-swoop
  :bind (;; ("H-s" . helm-swoop)
         ;; Give swoop additional bindings
         :map helm-swoop-map
              ("C-s" . swoop-action-goto-line-next)
              ("C-r" . swoop-action-goto-line-prev)))

;;;  helm-bm.el
;; https://github.com/jixiuf/helm-bm
(use-package helm-bm
  :commands (helm-bm))


;;;  helm-google.el
;; https://github.com/steckerhalter/helm-google
(use-package helm-google
  :commands (helm-google))


;;;  helm-hunks.el
;; https://github.com/torgeir/helm-hunks.el/blob/master/readme.md
(use-package helm-hunks
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :config
  (add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh))


;;;  helm-git-grep.el
;; https://github.com/yasuyk/helm-git-grep
(use-package helm-git-grep
  :commands (helm-git-grep
             helm-git-grep-from-isearch
             helm-git-grep-from-helm))


;;;  helm-elscreen.el
;; https://github.com/emacs-helm/helm-elscreen
(use-package helm-elscreen
  :commands (helm-elscreen)
  :bind ("C-; h" . helm-elscreen))


;;;  helm-directory.el
;; https://github.com/masasam/emacs-helm-directory
(use-package helm-directory
  :commands (helm-directory)
  :config
  (setq helm-directory-basedir "~"))


;;;  helm-tramp.el
;; https://github.com/masasam/emacs-helm-tramp
(use-package helm-tramp
  :commands (helm-tramp))
