;;; 500_yasnippet-related.el ---                     -*- lexical-binding: t; -*-

;;; yasnippet.el
;; https://github.com/joaotavora/yasnippet
;; http://joaotavora.github.io/yasnippet/
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
  ;; Functions to prompt for keys, templates, etc interactively.
  (add-to-list 'yas-prompt-functions
               'yas-popup-isearch-prompt)
  ;;
  ;; Activate globally.
  (yas-global-mode 1))
