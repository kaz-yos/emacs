;;;
;;; TEMPLATE-RELATED
;;;  yasnippet
;; https://github.com/capitaomorte/yasnippet
;; https://joaotavora.github.io/yasnippet
;; https://blog.alex-miller.co/emacs/spacemacs/2017/05/28/yasnippets.html/
;; http://fukuyama.co/yasnippet
;; http://d.hatena.ne.jp/kiwanami/20110224/1298526678
(use-package yasnippet
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
              ("C-x i v" . yas-visit-snippet-file))
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


;;;  yatemplate.el
;; https://github.com/mineo/yatemplate
;; http://emacs.rubikitch.com/yatemplate/
;; http://emacs.rubikitch.com/sd1602-autoinsert-yatemplate-yasnippet/
;; https://github.com/fommil/dotfiles/tree/master/.emacs.d/templates
(use-package yatemplate
  :disabled t
  :config
  ;; Template folder (default)
  (setq yatemplate-dir "~/.emacs.d/templates/")
  ;; Filename separator (Avoid : in macOS)
  (setq yatemplate-separator ";")
  ;; Fill auto-insert-alist
  ;; auto-insert-alist: A list specifying text to insert by default into a new file.
  (yatemplate-fill-alist))
