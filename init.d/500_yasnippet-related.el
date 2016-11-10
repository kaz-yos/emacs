;;; YAsnippet ; Automatic template inserting system.
;;
;; https://github.com/capitaomorte/yasnippet
;; http://fukuyama.co/yasnippet
;; http://d.hatena.ne.jp/kiwanami/20110224/1298526678
;;
(require 'yasnippet) ; elpa version active. el-get version required to suppress for some reason?
;;
;; auto-complete-yasnippet	; not sure if this is necessary. Inactive 2014-02-03
;; https://github.com/szunyog/.emacs.d/blob/master/auto-complete-yasnippet.el
;; (require 'auto-complete-yasnippet)
;;
(yas-global-mode 1)
;; Key-bind for expanding
;; Insert default snippet
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; Open a buffer to create a new snippet
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; View/edit snippets
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
;; Expand
(define-key yas-minor-mode-map (kbd "A-y") 'yas/expand)
;;
;; Use Popup isearch For Yasnippet Prompt 2014-01-??
;; http://iany.me/2012/03/use-popup-isearch-for-yasnippet-prompt/
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
;;
;;
;;; helm-c-yasnippet.el
;; https://github.com/emacs-jp/helm-c-yasnippet
;; http://rubikitch.com/2015/10/05/helm-c-yasnippet/
(use-package helm-c-yasnippet
  :bind ("C-c y" . helm-yas-complete)
  :config
  (setq helm-yas-space-match-any-greedy t))
;;
;;
;;; yatemplate.el
;; https://github.com/mineo/yatemplate
;; http://emacs.rubikitch.com/yatemplate/
(use-package yatemplate
  :config
  ;; Template folder (default)
  (setq yatemplate-dir "~/.emacs.d/templates")
  ;; Fill auto-insert-alist
  ;; auto-insert-alist: A list specifying text to insert by default into a new file.
  (yatemplate-fill-alist)
  ;;
  ;; From rubikitch example
  (defun find-file-hook--yatemplate ()
    "Activate snippet-mode upon opening a file in the template directory"
    (when (string-match "emacs.d/templates/" buffer-file-name)
      (let ((mode major-mode))
        (snippet-mode)
        (setq-local yas--guessed-modes (list mode)))))
  (add-hook 'find-file-hook 'find-file-hook--yatemplate)
  ;;
  (defun after-save-hook--yatemplate ()
    "Update auto-insert-alist after saving a yatemplate file"
    (when (string-match "emacs.d/templates/" buffer-file-name)
      (yatemplate-fill-alist)))
  (add-hook 'after-save-hook 'after-save-hook--yatemplate))
