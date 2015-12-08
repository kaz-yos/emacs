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
     :isearch t
     )))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
;;
;;
;;; helm-c-yasnippet.el		; helm source for yasnippet.el
;; config in the el file
;; (require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t) ;[default: nil]
(global-set-key (kbd "C-c y") 'helm-yas-complete)
;; (yas-global-mode 1)
;; (yas-load-directory "<path>/<to>/snippets/")




