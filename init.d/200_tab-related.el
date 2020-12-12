;;; 200_tab-related.el ---                           -*- lexical-binding: t; -*-

;;;
;;; tab-bar-mode.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html
(use-package tab-bar
  ;; Available in emacs 27 and later.
  :if (not (version< emacs-version "27.0"))
  :demand t
  :commands (tab-bar-new-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :bind-keymap (("C-c l" . tab-bar-map))
  :bind (("C-M-o" . tab-bar-switch-to-next-tab)
         :map tab-bar-map
         ("c" . my-tab-bar-create)
         ("C" . my-tab-bar-clone)
         ("n" . tab-bar-switch-to-next-tab)
         ("p" . tab-bar-switch-to-prev-tab))
  ;;
  :config
  ;; Define functions
  ;; tab-bar version of separate buffer list filter
  ;; https://github.com/wamei/elscreen-separate-buffer-list/issues/8
  (defun my-tab-bar-buffer-name-filter (buffer-names)
    "Filter BUFFER-NAMES by the current tab's buffer list

It should be used to filter a list of buffer names created by
other functions, such as `helm-buffer-list'."
    (let ((buffer-names-to-keep
           ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
           (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                   (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab))))))
      (seq-filter (lambda (elt)
                    (member elt buffer-names-to-keep))
                  buffer-names)))
  ;;
  (defun my-tab-bar-create (&optional arg)
    "Create a new tab with cleaned buffer lists.

ARG is directly passed to `tab-bar-new-tab'.
Only the current buffer is kept in the `buffer-list'.
`buried-buffer-list' is cleared.
This is similar to `elscreen-create'."
    (interactive)
    (tab-bar-new-tab arg)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
    (set-frame-parameter nil 'buffer-list (list (current-buffer)))
    (set-frame-parameter nil 'buried-buffer-list nil))
  ;;
  (defun my-tab-bar-clone (&optional arg)
    "Create a new tab by cloning the current tab.

ARG is directly passed to `tab-bar-new-tab'.
This does not touch the `buffer-list' and `buried-buffer-list'.
This is similar to `elscreen-clone'."
    (interactive)
    ;; If nil, duplicate the contents of the tab that was active
    ;; before calling the command that adds a new tab.
    (let ((tab-bar-new-tab-choice nil))
      (tab-bar-new-tab arg)))
  ;;
  ;; Do not show buttons.
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)
  ;; Show absolute numbers on tabs.
  (setq tab-bar-tab-hints t)
  ;; String that delimits tabs.
  (setq tab-bar-separator " ")
  ;; Defines what to show in a new tab.
  (setq tab-bar-new-tab-choice "*scratch*")
  ;; Defines where to create a new tab.
  (setq tab-bar-new-tab-to 'rightmost)
  ;;
  ;; Activate
  (tab-bar-mode +1))
