;;; Window management


;;;
;;; window.el.gz
;; This file does not provide a package name.
;; https://www.reddit.com/r/emacs/comments/hzhkk1/what_simple_key_remapping_has_transformed_your/
(bind-key "M-o" 'other-window)


;;;
;;; winner-mode to restore previous window configurations
;; http://www.emacswiki.org/emacs/WinnerMode
;; Default: C-c <left> to undo window rearragement. C-c <right> to redo.
(use-package winner
  :demand
  :bind (("M-<left>" . winner-undo)
         ("M-<left>" . winner-undo))
  :config
  (winner-mode t))


;;;
;;; windmove.el
;; Built-in
(use-package windmove
  :bind (
         :map mode-specific-map
         ("o" . windmove-right))
  :commands (windmove-left
             windmove-right
             windmove-up
             windmove-down)
  :config
  ;; Whether movement off the edge of the frame wraps around.
  (setq windmove-wrap-around t))


;;;
;;; Useful window shortcuts
;; http://stackoverflow.com/questions/2901198/useful-keyboard-shortcuts-and-tips-for-ess-r
;; C-tab to switch to other window.
;; (global-set-key [C-tab] 'other-window)
;;
;; C-tab to split or switch to other window. Book by rubikitch p74
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
;; http://stackoverflow.com/questions/916797/emacs-global-set-key-to-c-tab
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    ;; When there's only one window, split horizontally.
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "<C-tab>") 'other-window-or-split)
(bind-key "<tab>" 'other-window-or-split my-key-map)
;; Reversal
;; http://stackoverflow.com/questions/143072/in-emacs-what-is-the-opposite-function-of-other-window-c-x-o
(global-set-key (kbd "<C-S-tab>") 'previous-multiframe-window)
(bind-key "<backtab>" 'previous-multiframe-window my-key-map)
;;
;; Select the previously-selected window in emacs
;; http://stackoverflow.com/questions/7937395/select-the-previously-selected-window-in-emacs
(defun switch-to-previous-buffer-possibly-creating-new-window ()
  (interactive)
  (pop-to-buffer (other-buffer (current-buffer) t)))


;;;
;;; Toggle window split layout
;; http://stackoverflow.com/questions/14881020/emacs-shortcut-to-switch-from-a-horizontal-split-to-a-vertical-split-in-one-move
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(defalias 'my-transpose-windows 'toggle-window-split)


;;;
;;; Scroll window with C-t/C-v
;; transpose-char changed to cua-scroll-down
;; C-t to scroll down, C-v to scroll u
(if (boundp 'cua-scroll-down)
    (bind-key* "C-t" 'cua-scroll-down)
  (bind-key* "C-t" 'scroll-down-command))
;;
;; Scroll just one line when hitting bottom of window
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-conservatively 10000)
;;
;; Scroll one line at a time (less "jumpy" than defaults)
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))     ; one line at a time
;;(setq mouse-wheel-progressive-speed nil)               ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                      ; scroll window under mouse
;;(setq scroll-step 1)                                   ; keyboard scroll one line at a time
;;


;;;
;;; Scroll other window with M-up/M-down
;; Conflict with paredit. Use C-M-(S)-v
;; (global-set-key [M-up]   'scroll-other-window-down)
;; (global-set-key [M-down] 'scroll-other-window)
(global-set-key (kbd "M-<up>")   'scroll-other-window-down)
(global-set-key (kbd "M-<down>") 'scroll-other-window)
(global-set-key (kbd "C-M-t")    'scroll-other-window-down)
(global-set-key (kbd "C-M-v")    'scroll-other-window)


;;;
;;; Scroll margin
;; http://pragmaticemacs.com/emacs/use-scroll-margins-to-give-context-to-your-cursor/
(setq scroll-margin 2)


;;; Do not open a new frame opening a file from Finder
;; System-specific configuration
;; http://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
(when (eq system-type 'darwin)
  ;; Do not open a new frame opening a file from Finder
  ;; http://stackoverflow.com/questions/6068819/alias-to-make-emacs-open-a-file-in-a-new-buffer-not-frame-and-be-activated-com
  (setq ns-pop-up-frames nil))
