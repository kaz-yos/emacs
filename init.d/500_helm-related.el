;;; Helm (Anything successor)
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688		; arabiki
;; http://d.hatena.ne.jp/a_bicky/20140125/1390647299		; arabiki 2
;; http://sleepboy-zzz.blogspot.com/2012/09/anythinghelm.html	; in general
;; http://d.hatena.ne.jp/syohex/20121207/1354885367		; some useful tips


;; Temporary fix for name change
;; https://github.com/emacs-helm/helm/commit/8cb7a9e0b98175845a68309e2dd9850bd0f02a28
(defalias 'helm--make-source 'helm-make-source)


;; Activate helm
(require 'helm-config)
(require 'helm-command)
;;
;;
(setq helm-idle-delay             0.3
      helm-input-idle-delay       0.3
      helm-candidate-number-limit 200
      helm-M-x-always-save-history t)
;;
;; Disabled because it was giving an error 2013-11-22
(setq helm-locate-command "")
;;
(let ((key-and-func
       `(;;(,(kbd "C-x C-f") helm-find-files)
	 (,(kbd "M-x")	   helm-M-x)
	 (,(kbd "C-z")	   helm-for-files)
	 (,(kbd "M-y")	   helm-show-kill-ring)
	 (,(kbd "C-x b")   helm-buffers-list)
	 (,(kbd "C-x C-r") helm-recentf)
	 ;;
         (,(kbd "C-^")	   helm-c-apropos)
         (,(kbd "C-;")	   helm-resume)
         (,(kbd "M-s")	   helm-occur)
	 (,(kbd "M-z")	   helm-do-grep)
         (,(kbd "C-S-h")   helm-descbinds)
	 )))
  (loop for (key func) in key-and-func
        do (global-set-key key func)))
;;
;; (define-key global-map (kbd "C-x b")   'helm-buffers-list)
;; (define-key global-map (kbd "C-x C-r") 'helm-recentf)
;;
;; helm for isearch 2014-02-01
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
;;
;; Emulate `kill-line' in helm minibuffer
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))
;;
;;
;;; debug
;; (defvar *helm for files*) in helm.el
;; 2014-09-21 Add the following to test how auctex works with this
;; (setq helm-buffer "*helm for files*")



;;;
;;; Optional helm packages
;;
;;; helm-descbinds.el
;; Replace describe-bindings with helm interface
;; http://emacs-jp.github.io/packages/helm/helm-descbinds.html
;; http://d.hatena.ne.jp/buzztaiki/20081115/1226760184 (anything version)
(require 'helm-descbinds)
(helm-descbinds-mode)
;;
;;
;;; helm-R.el
(require 'helm-R)
;;
;;
;;; helm-migemo.el
;; http://sleepboy-zzz.blogspot.com/2013/02/helm-migemo.html	; helm-migemo
(when (eq system-type 'darwin)
    ;; Mac-only
    (require 'helm-migemo))
;;
;;
;;; wgrep-helm.el  2014-01-14
;; Writable helm-grep-mode buffer and apply the changes to files
(require 'wgrep-helm)
;;
;;
;;; helm-ag.el
;; https://github.com/syohex/emacs-helm-ag
;; http://qiita.com/l3msh0@github/items/97909d6e2c92af3acc00
(require 'helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-thing-at-point 'symbol)
;;
;;
;;; helm-open-github 2014-02-01 OAutho required
;; http://shibayu36.hatenablog.com/entry/2013/01/18/211428
(require 'helm-open-github)
;; (global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
;; (global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
;; (global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
;;
;;
;;; ac-helm.el		; Helm interface for auto-complete
(require 'ac-helm)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)
;;
;;
;;; helm-mode-manager.el		; Select and toggle major and minor modes with helm
(require 'helm-mode-manager)
;;
;;
;;; helm-package.el	; Listing ELPA packages with helm interface
(require 'helm-package)
;;
;;
;;; helm-themes.el	; Color theme selection with helm interface
(require 'helm-themes)
;;
;;
;;; helm-dash.el
;; http://fukuyama.co/helm-dash
;; http://kapeli.com/dash
(require 'helm-dash)
