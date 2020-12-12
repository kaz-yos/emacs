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
  :hook ((after-init . my-tab-bar-setup))
  :bind-keymap (("C-c l" . tab-prefix-map))
  :bind (("C-M-o" . tab-bar-switch-to-next-tab)
         ;; `tab-bar-map' is for mouse actions on the tab bar.
         ;; `tab-prefix-map' is defined in subr.el.
         :map tab-prefix-map
         ("c" . my-tab-bar-create)
         ("C" . my-tab-bar-clone)
         ("n" . tab-bar-switch-to-next-tab)
         ("p" . tab-bar-switch-to-prev-tab)
         ("k" . tab-bar-close-tab)
         ("K" . tab-bar-close-other-tabs)
         ("s" . tab-bar-select-tab-by-name)
         ("r" . tab-bar-rename-tab)
         ("1" . my-tab-bar-select-tab-1)
         ("2" . my-tab-bar-select-tab-2)
         ("3" . my-tab-bar-select-tab-3)
         ("4" . my-tab-bar-select-tab-4)
         ("5" . my-tab-bar-select-tab-5)
         ("6" . my-tab-bar-select-tab-6)
         ("7" . my-tab-bar-select-tab-7)
         ("8" . my-tab-bar-select-tab-8)
         ("9" . my-tab-bar-select-tab-9))
  ;;
  :config
  ;; Define functions
  (defun my-tab-bar-setup ()
    "Set up several tabs at startup."
    ;; Create 2
    (progn (my-tab-bar-create)
           (tab-bar-rename-tab "init.d")
           (find-file (concat user-emacs-directory "init.d"))
           (split-window nil nil 'left))
    ;; Create Tab 3
    (progn (my-tab-bar-create)
           (find-file (if (file-exists-p "~/Dropbox/documents")
                          "~/Dropbox/documents"
                        "~"))
           (split-window nil nil 'left))
    ;; Go back to Tab 1
    (my-tab-bar-select-tab-1))
  ;;
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
  (defun my-create-tab-bar-select-tab (num)
    `(lambda ()
       (interactive)
       (tab-bar-select-tab ,num)))
  (fset 'my-tab-bar-select-tab-1 (my-create-tab-bar-select-tab 1))
  (fset 'my-tab-bar-select-tab-2 (my-create-tab-bar-select-tab 2))
  (fset 'my-tab-bar-select-tab-3 (my-create-tab-bar-select-tab 3))
  (fset 'my-tab-bar-select-tab-4 (my-create-tab-bar-select-tab 4))
  (fset 'my-tab-bar-select-tab-5 (my-create-tab-bar-select-tab 5))
  (fset 'my-tab-bar-select-tab-6 (my-create-tab-bar-select-tab 6))
  (fset 'my-tab-bar-select-tab-7 (my-create-tab-bar-select-tab 7))
  (fset 'my-tab-bar-select-tab-8 (my-create-tab-bar-select-tab 8))
  (fset 'my-tab-bar-select-tab-9 (my-create-tab-bar-select-tab 9))
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
