;;; 500_yasnippet-related.el ---                     -*- lexical-binding: t; -*-

;;; yasnippet.el
;; https://github.com/capitaomorte/yasnippet
;; https://joaotavora.github.io/yasnippet
;; https://blog.alex-miller.co/emacs/spacemacs/2017/05/28/yasnippets.html/
;; http://fukuyama.co/yasnippet
;; http://d.hatena.ne.jp/kiwanami/20110224/1298526678
;; https://www.youtube.com/watch?v=xmBovJvQ3KU
(use-package yasnippet
  :ensure t
  :defer 2
  :diminish  yas-minor-mode
  :bind (:map yas-minor-mode-map
              ;; Insert with a menu
              ("C-x i i" . yas-insert-snippet)
              ;; Expand at point
              ("C-x i e" . yas-expand)
              ;; Open a buffer to create a new snippet
              ("C-x i n" . yas-new-snippet)
              ;; View/edit snippets
              ("C-x i v" . yas-visit-snippet-file)
              ;; Display snippets for each table.
              ("C-x i d" . yas-describe-tables))
  :config
  ;; Use Popup isearch for Yasnippet Prompt.
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
  ;; Activate globally.
  (yas-global-mode 1))
