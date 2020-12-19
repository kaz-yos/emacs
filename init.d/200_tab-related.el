;;; 200_tab-related.el ---                           -*- lexical-binding: t; -*-

;;;
;;; tab-bar-mode.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html
;; Author's configuration
;; https://github.com/link0ff/emacs-init/blob/master/README.org#tabs-with-tab-bar-and-tab-lines
;; tab-bar-mode - no visual tabs? (not implemented in macOS, yet).
;; https://www.reddit.com/r/emacs/comments/fdbsc8/tabbarmode_no_visual_tabs/
;; Use example
;; https://www.youtube.com/watch?v=C7ZlNRbWdVI
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
         ("u" . tab-bar-undo-close-tab)
         ;; ("D" . display-buffer-in-new-tab) ; not a user command
         ;;
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
    ;; Create Tab 2
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
  ;; This looks like a more sophisticated implementation.
  ;; https://github.com/ROCKTAKEY/tab-bar-display
  (defun my-tab-bar-string ()
    "Obtain the text version of the tab bar."
    (let ((tab-bar-keymap (tab-bar-make-keymap-1)))
      (thread-last tab-bar-keymap
        ;; Drop keymap
        (cdr)
        ;; Keep ones that have tab as the first element
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Match-Data.html
        (seq-filter (lambda (elt)
                      (string-match "^tab-\\|^current-tab$"
                                    (symbol-name (car elt)))))
        ;; Extract names with a special emphasis on the current tab.
        (seq-map (lambda (elt)
                   (if (not (string-match "^current-tab$"
                                          (symbol-name (car elt))))
                       (substring-no-properties (nth 2 elt))
                     ;; Current tab special handling
                     (concat
                      "<<"
                      (substring-no-properties (nth 2 elt))
                      ">>"))))
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html
        ;; Outer () is necessary to ensure correct thread-last'ing
        ((lambda (seq)
           (mapconcat #'identity seq tab-bar-separator))))))
  ;;
  (defvar *my-tab-bar-as-string*
    ""
    "Variable to hold curent tab names as a string")
  ;;
  (defun my-update-tab-bar-as-string ()
    "Update *my-tab-bar-as-string* variable"
    (interactive)
    (setq *my-tab-bar-as-string* (my-tab-bar-string)))
  ;; Functions called during redisplay when window configuration has changed.
  (add-hook 'window-configuration-change-hook 'my-update-tab-bar-as-string)
  ;;
  ;; Set frame title format as combination of current tabs and buffer/path
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Properties.html
  ;; http://kitchingroup.cheme.cmu.edu/blog/2014/09/14/Colorized-text-in-Emacs/
  (setq frame-title-format '(:eval (propertize
                                    ;; String
                                    (concat *my-tab-bar-as-string*
                                            "    ||    "
                                            (cond
                                             (buffer-file-name
                                              (abbreviate-file-name buffer-file-name))
                                             ((string-equal major-mode "dired-mode")
                                              (dired-current-directory))
                                             (t "%b")))
                                    ;; Properties: a sequence of PROPERTY VALUE pairs
                                    'face '(:foreground "white"))))
  ;;
  (defun my-add-buffer-info (tab-bar-keymap)
    "Add an additional item to tab-bar keymap."
    (append tab-bar-keymap
            `((buffer-file-name menu-item
                                ,(concat
                                  " ||  "
                                  (cond
                                   (buffer-file-name
                                    (abbreviate-file-name buffer-file-name))
                                   ((string-equal major-mode "dired-mode")
                                    (dired-current-directory))
                                   (t (buffer-name))))
                                ignore))))
  ;; Use only in terminal. It is done through frame-title-format in GUI.
  (unless (display-graphic-p)
    (advice-add 'tab-bar-make-keymap-1
                :filter-return #'my-add-buffer-info))
  ;;
  ;; Use the projectile project name for the tab name.
  ;; https://github.com/toyboot4e/dotfiles/blob/master/editor/emacs/elisp/ide.el#L172-L178
  (with-eval-after-load "projectile"
    (defun my-tab-bar-tab-name-current-project-or-buffer ()
      (let ((project-name (projectile-project-name)))
        (if (string-equal project-name "-")
            (tab-bar-tab-name-current)
          project-name)))
    ;; Function to get a tab name.
    (setq tab-bar-tab-name-function
          #'my-tab-bar-tab-name-current-project-or-buffer))
  ;;
  ;; Defines when to show the tab bar.
  (setq tab-bar-show t)
  ;; Do not show buttons.
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)
  ;; Show absolute numbers on tabs.
  (setq tab-bar-tab-hints t)
  ;; String that delimits tabs. More spaces are nice for the frame-title.
  (setq tab-bar-separator (if (display-graphic-p)
                              "    "
                            " "))
  ;; Defines what to show in a new tab.
  (setq tab-bar-new-tab-choice "*scratch*")
  ;; Defines where to create a new tab.
  (setq tab-bar-new-tab-to 'rightmost)
  ;;
  ;; Buttons for the tab history. Originally < >.
  (setq tab-bar-back-button "")
  (setq tab-bar-forward-button "")
  ;;
  ;; Activate
  (tab-bar-mode +1)
  (tab-bar-history-mode +1))


;;;
;;; tab-line-mode.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Line.html
(use-package tab-line
  ;; To use `my-tab-bar-buffer-name-filter' defined in the tab-bar configuration.
  :after tab-bar
  :config
  ;; Project based organization
  ;; http://amitp.blogspot.com/2020/06/emacs-prettier-tab-line.html
  ;; Show only the buffers from the current project, sorted alphabetically.
  ;; `tab-line-tabs-function' can be set to group mode, which uses
  ;; `tab-line-tabs-buffer-group-function' to choose the group name and
  ;; `tab-line-tabs-buffer-group-sort-function' to sort the buffers in the group.
  (defun my-tab-line-buffer-group (buffer)
    "Use the project.el name for the buffer group"
    (with-current-buffer buffer
      (let ((prj (project-current)))
        (when prj
          (replace-regexp-in-string "/$" ""
                                    (car (project-roots prj)))))))
  ;;
  (defun my-buffer-name-sort (a b)
    (string< (buffer-name a)
             (buffer-name b)))
  ;;
  (defun my-buffer-list-current-tab-bar ()
    "Obtain a list of buffers in the current tab bar."
    (let ((buffer-names-to-keep
           (my-tab-bar-buffer-name-filter
            (seq-map #'buffer-name (tab-line-tabs-buffer-list)))))
      (seq-filter (lambda (buffer)
                    (member (buffer-name buffer) buffer-names-to-keep))
                  (tab-line-tabs-buffer-list))))
  ;;
  (defun my-buffer-list-current-project-current-tab-bar ()
    "Obtain a list of buffers in the current buffer's project.

The list is also restricted to the current tab-bar."
    (let ((buff-lst (my-buffer-list-current-tab-bar))
          (cur-prj (my-tab-line-buffer-group (current-buffer))))
      (seq-filter (lambda (buffer)
                    (string-equal (my-tab-line-buffer-group buffer)
                                  cur-prj))
                  buff-lst)))
  ;;
  (defun my-tab-line-tab-next ()
    "Switch to the next tab in the tab-line"
    (interactive)
    (let* ((tabs (funcall tab-line-tabs-function))
           (tabs-max-index (1- (length tabs)))
           (cur-buff (current-buffer))
           (cur-buff-index (cl-position cur-buff tabs))
           (next-buff-index (1+ cur-buff-index)))
      (cond
       ;; If only one tab, don't do anything.
       ((<= tabs-max-index 0) nil)
       ;; If going beyond the max, go to index 0.
       ((> next-buff-index tabs-max-index) (switch-to-buffer (seq-elt tabs 0)))
       ;; Otherwise, go to the next index.
       (t (switch-to-buffer (seq-elt tabs next-buff-index))))))
  ;;
  (defun my-tab-line-tab-previous ()
    "Switch to the previous tab in the tab-line"
    (interactive)
    (let* ((tabs (funcall tab-line-tabs-function))
           (tabs-max-index (1- (length tabs)))
           (cur-buff (current-buffer))
           (cur-buff-index (cl-position cur-buff tabs))
           (prev-buff-index (1- cur-buff-index)))
      (cond
       ;; If only one tab, don't do anything.
       ((<= tabs-max-index 0) nil)
       ;; If going beyond 0, go to the max index.
       ((< prev-buff-index 0) (switch-to-buffer (seq-elt tabs tabs-max-index)))
       ;; Otherwise, go to the previous index.
       (t (switch-to-buffer (seq-elt tabs prev-buff-index))))))
  ;;
  ;; Function to return a global list of buffers.
  ;; Used only for `tab-line-tabs-mode-buffers' and `tab-line-tabs-buffer-groups'.
  (setq tab-line-tabs-buffer-list-function
        #'my-buffer-list-current-project-current-tab-bar)
  ;;
  ;; Function to sort buffers in group.
  (setq tab-line-tabs-buffer-group-sort-function #'my-buffer-name-sort)
  ;; Function to put a buffer to the group.
  (setq tab-line-tabs-buffer-group-function #'my-tab-line-buffer-group)
  ;; Function to get a list of tabs to display in the tab line.
  ;; (setq tab-line-tabs-function #'tab-line-tabs-buffer-groups)
  ;; returns a list of buffers associated with the selected window.
  ;; (setq tab-line-tabs-function #'tab-line-tabs-window-buffers)
  ;; return a list of buffers with the same major mode
  (setq tab-line-tabs-function #'tab-line-tabs-mode-buffers)
  ;;
  ;; Minimize visual clutters
  (setq tab-line-separator nil)
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil)
  ;;
  (global-tab-line-mode +1))
