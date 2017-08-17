;;; Mode-line-related


;;;
;;; powerline.el
;; https://github.com/milkypostman/powerline
;; http://shibayu36.hatenablog.com/entry/2014/02/11/160945
(use-package powerline
  :disabled t
  :config
  ;; https://github.com/shibayu36/emacs/blob/8376ba4e4b4d5cb4668e848c9a494b287232ea6f/emacs.d/inits/01-mode-line.el
  (set-face-attribute 'mode-line nil
                      :foreground "black"
                      :background "gray90"
                      :box nil)
  (set-face-attribute 'powerline-active1 nil
                      :foreground "black"
                      :background "gray80"
                      :inherit 'mode-line)
  (set-face-attribute 'powerline-active2 nil
                      :foreground "black"
                      :background "gray70"
                      :inherit 'mode-line)
  ;; Configure at the end
  (powerline-default-theme))


;;;
;;; spaceline.el
;; Powerline theme from Spacemacs
;; https://github.com/TheBB/spaceline
(use-package spaceline
  :disabled t
  :config
  ;; https://github.com/TheBB/spaceline#the-default-themes
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))


;;;
;;; all-the-icons.el
;; https://github.com/domtronn/all-the-icons.el
;; https://github.com/domtronn/all-the-icons.el/wiki
;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line
(use-package all-the-icons
  :disabled t
  :config
  (defun custom-modeline-modified
      "Modified or Read Only

This snippet displays a chain icon when the current file is saved, a broken chain when it is modified and a pad lock when the file is read only."
    ((let* ((config-alist
             '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
               ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
               ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
            (result (cdr (assoc (format-mode-line "%*") config-alist))))
       (propertize (apply (cadr result) (cddr result))
                   'face `(:family ,(funcall (car result)))))))
  ;;
  (defun custom-modeline-window-number ()
    "Window Numbers

This snippet displays a number icon for the number of the current window. This is designed to work with the Window Numbering package."
    (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
                'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0))
                'display '(raise 0.0)))
  ;;
  (defun custom-modeline-mode-icon ()
    "Mode Icon

This snippet displays the Developer Icon for the mode of that buffers file."
    (format " %s"
            (propertize icon
                        'help-echo (format "Major-mode: `%s`" major-mode)
                        'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))
  ;;
  (defun custom-modeline-region-info ()
    "Region Marking

This snippet displays useful information on the current marked region, i.e. number of lines and characters marked."
    (when mark-active
      (let ((words (count-lines (region-beginning) (region-end)))
            (chars (count-words (region-end) (region-beginning))))
        (concat
         (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                     'face `(:family ,(all-the-icons-octicon-family))
                     'display '(raise -0.0))
         (propertize (format " (%s, %s)" words chars)
                     'face `(:height 0.9))))))
  ;;
  (defun -custom-modeline-github-vc ()
    "Version Control Icon Helper for Git"
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
       " · "
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9)))))

  (defun -custom-modeline-svn-vc ()
    "Version Control Icon Helper for SVN"
    (let ((revision (cadr (split-string vc-mode "-"))))
      (concat
       (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
       (propertize (format " · %s" revision) 'face `(:height 0.9)))))

  (defun custom-modeline-icon-vc ()
    "Version Control Icon

This snippet displays information about the current buffers version control system. Currently, it only supports SVN & Git for including icons."
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
       ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
       (t (format "%s" vc-mode)))))
  ;;
  (defun custom-modeline-flycheck-status ()
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
                                "✔ No Issues"))
                   (`running     "⟲ Running")
                   (`no-checker  "⚠ No Checker")
                   (`not-checked "✖ Disabled")
                   (`errored     "⚠ Error")
                   (`interrupted "⛔ Interrupted")
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))
  ;;
  (defvar powerline/upgrades nil)
  (defun powerline/count-upgrades ()
    (let ((buf (current-buffer)))
      (package-list-packages-no-fetch)
      (with-current-buffer "*Packages*"
        (setq powerline/upgrades (length (package-menu--find-upgrades))))
      (switch-to-buffer buf)))
  (advice-add 'package-menu-execute :after 'powerline/count-upgrades)
  (defun custom-modeline-package-updates ()
    "Number of Packages to Update

This snippet displays the number of packages that you last needed to update. This currently works every time you refresh your package archive list, so the number can get stale pretty quickly."
    (let ((num (or powerline/upgrades (powerline/count-upgrades))))
      (when (> num 0)
        (propertize
         (concat
          (propertize (format "%s" (all-the-icons-octicon "package"))
                      'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
                      'display '(raise -0.1))
          (propertize (format " %d updates " num)
                      'face `(:height 0.9)))
         'help-echo "Open Packages Menu"
         'mouse-face '(:box 1)
         'local-map (make-mode-line-mouse-map
                     'mouse-1 (lambda () (interactive) (package-list-packages)))))))
  ;;
  (defun custom-modeline-suntime ()
    (if (and (boundp 'yahoo-weather-info) yahoo-weather-mode)
        (concat
         (format "%s "(yahoo-weather-info-format yahoo-weather-info "%(sunrise-time)"))
         (format "%s  " (all-the-icons-wicon "sunrise" :height 0.5 :v-adjust -0.1))
         (format "%s "(yahoo-weather-info-format yahoo-weather-info "%(sunset-time)"))
         (format "%s "(all-the-icons-wicon "sunset" :height 0.5 :v-adjust -0.1)))
      ""))
  (defun custom-modeline-weather ()
    "Weather Summary

This snippet displays the current time, the sunrise, sunset, current weather and the current temperature. It uses (and depends on) the Yahoo Weather package."
    (if (and (boundp 'yahoo-weather-info) yahoo-weather-mode)
        (let* ((weather (yahoo-weather-info-format yahoo-weather-info format))
               (icon (all-the-icons-icon-for-weather (downcase weather)))
               (family (if (> (length icon) 2)
                           (face-attribute 'default :family)
                         (all-the-icons-wicon-family))))
          (propertize (format " %s " icon)
                      'help-echo weather
                      'face `(:height 1.0 :family ,family)
                      'display '(raise 0.1)))
      ""))
  ;;
  (defun custom-modeline-time ()
    "Time with an Icon Clock

This snippet displays the current time with a little clock icon which represents the current Hour (i.e. a clock face where the minute hand doesn't move)"
    (let* ((hour (string-to-number (format-time-string "%I")))
           (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
      (concat
       (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
       (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))
  ;;
  ;; Configure
  (setq mode-line-format '("%e" (:eval
                                 (concat
                                  (custom-modeline-modified)
                                  (custom-modeline-window-number)
                                  (custom-modeline-mode-icon)
                                  (custom-modeline-icon-vc)
                                  (custom-modeline-region-info)
                                  (custom-modeline-flycheck-status)
                                  (custom-modeline-suntime)
                                  (custom-modeline-weather)
                                  (custom-modeline-time))))))



;;;
;;; Clean mode-line when switching major modes
;; https://github.com/shibayu36/emacs/blob/8376ba4e4b4d5cb4668e848c9a494b287232ea6f/emacs.d/inits/01-mode-line.el
(defvar mode-line-cleaner-alist
  '(;; For minor-mode, first char is 'space'
    (abbrev-mode            . "")
    (anzu-mode              . "")
    (company-mode           . "")
    (eldoc-mode             . "")
    (elisp-slime-nav-mode   . "")
    (flymake-mode           . "")
    (git-gutter-mode        . "")
    (guide-key-mode         . "")
    (icicle-mode            . "")
    (ivy-mode               . "")
    (magit-auto-revert-mode . "")
    (paredit-mode           . "")
    (super-save-mode        . "")
    (undo-tree-mode         . "")
    (yas-minor-mode         . "")
    ;;
    ;; Major modes
    (dired-mode             . "Dir")
    (emacs-lisp-mode        . "elisp")))
;;
(defun clean-mode-line ()
  "Clean mode-line expressions"
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))
;;
(add-hook 'after-change-major-mode-hook 'clean-mode-line)
