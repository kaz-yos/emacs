;;; spell check related

;;;
;;; ispell.el (built-in)
;; Spell checking in Emacs (ispell, flyspell, hunspell, and languagetool)
;; https://joelkuiper.eu/spellcheck_emacs
;;
;; brew install hunspell
;; Dictionary files (*.aff and *.dic) should be placed in
;; ~/Library/Spelling/ or /Library/Spelling/.  Homebrew itself
;; provides no dictionaries for Hunspell, but you can download
;; compatible dictionaries from other sources, such as
;; https://wiki.openoffice.org/wiki/Dictionaries .
;;
;; Spell check with multiple dictionaries (hunspell)
;; http://emacs.stackexchange.com/questions/21378/spell-check-with-multiple-dictionaries
(use-package ispell
  :commands (ispell-word
             ispell-region
             ispell-buffer)
  :config
  ;; Use best program available
  (let ((hunspell-file (executable-find "hunspell"))
        (aspell-file (executable-find "aspell"))
        (ispell-file (executable-find "ispell")))
    (cond
     (hunspell-file (setq ispell-program-name hunspell-file))
     (aspell-file (setq ispell-program-name aspell-file))
     (ispell-file (setq ispell-program-name ispell-file))))
  ;;
  (setq ispell-dictionary "en_US")
  ;; http://stackoverflow.com/questions/2376113/personal-dictionaries-in-emacs-flyspell-mode
  (setq ispell-personal-dictionary "~/.emacs.d/misc/aspell.en.pws")
  ;;
  ;; Ignore Japanese
  ;; http://keisanbutsuriya.blog.fc2.com/blog-entry-60.html
  ;; Alist expressing beginning and end of regions not to spell check.
  ;; The alist key must be a regular expression.
  ;; Valid forms include:
  ;; (KEY) - just skip the key.
  ;; (KEY . REGEXP) - skip to the end of REGEXP.  REGEXP may be string or symbol.
  ;; (KEY REGEXP) - skip to end of REGEXP.  REGEXP must be a string.
  ;; (KEY FUNCTION ARGS) - FUNCTION called with ARGS returns end of region.
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))


;;;
;;; flyspell.el (built-in)
;;
(use-package flyspell
  :ensure t
  :commands (flyspell-goto-next-error
             flyspell-mode
             turn-on-flyspell
             turn-off-flyspell)
  :config
  ;; Unset some key bindings to avoid collisions
  (setq flyspell-auto-correct-binding nil)
  (eval-after-load "flyspell"
    #'(lambda ()
        ;; Drop unnecessary key bindings
        ;; http://stackoverflow.com/questions/16084022/emacs-flyspell-deactivate-c-key-binding
        (define-key flyspell-mode-map (kbd "C-,") nil)
        (define-key flyspell-mode-map (kbd "C-.") nil)
        (define-key flyspell-mode-map (kbd "C-;") nil)
        (define-key flyspell-mode-map (kbd "C-c $") nil)
        (define-key flyspell-mode-map (kbd "C-M-i") nil)
        ;; Bind to less problematics ones
        (define-key flyspell-mode-map (kbd "A-,") 'flyspell-goto-next-error)
        (define-key flyspell-mode-map (kbd "A-.") 'flyspell-popup-correct))))
;;
;;
;;; flyspell-popup.el
;; https://github.com/xuchunyang/flyspell-popup
(use-package flyspell-popup
  :commands (flyspell-popup-correct)
  :bind ("s-c" . flyspell-popup-correct)
  :config
  ;;
  (defun turn-on-off-flyspell-unless-already-on (oldfun)
    "Turn flyspell on :before, and turn off :after unless already on

This is meant to be an :around advice to flyspell-popup-correct,
which has no argument of its own."
    (if flyspell-mode
        ;; If already on, just run the original function
        (funcall oldfun)
      ;; Otherwise,
      (turn-on-flyspell)
      ;; Call the original function
      (funcall oldfun)
      (turn-off-flyspell)))
  ;; :around
  ;; (advice-add 'flyspell-popup-correct
  ;;             :around
  ;;             #'turn-on-off-flyspell-unless-already-on)
  ;;
  ;; :around advice seems to break key binding in popup menu
  (advice-add 'flyspell-popup-correct
              :before
              #'turn-on-flyspell)
  )
