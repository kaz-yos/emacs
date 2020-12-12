;;; 200_tab.el ---                                   -*- lexical-binding: t; -*-

;;;
;;; tab-bar-mode.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html
(use-package tab-bar
  :config
  ;; tab-bar version of separate buffer list filter
  ;; https://github.com/wamei/elscreen-separate-buffer-list/issues/8
  (defun my-tab-bar-buffer-name-filter (buffer-names)
    "Filter BUFFER-NAMES by the current tab's buffer list"
    (let ((buffer-names-to-keep
           ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
           (mapcar #'buffer-name
                   (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                           (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab)))))))
      (seq-filter (lambda (elt)
                    (member elt buffer-names-to-keep))
                  buffer-names)))
  ;;
  ;; Activate
  (tab-bar-mode))
