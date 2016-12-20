;;; flyspell-mode related

;;;
;;; Configuration helpful links
;; http://d.hatena.ne.jp/yutoichinohe/20140120/1390225624
;; http://keisanbutsuriya.blog.fc2.com/blog-entry-60.html
;; https://joelkuiper.eu/spellcheck_emacs
;; http://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary

;;;
;;; Configure aspell if it exists
;; brew install aspell --with-lang-en
(let ((aspell-file "/usr/local/bin/aspell"))
  (when (file-exists-p aspell-file)
    (setq ispell-program-name aspell-file)))


;;;
;;; ispell.el (built-in)
;; (global-set-key (kbd "s-c") 'ispell-word)
;;
(use-package ispell
  :commands (ispell-word
             ispell-region
             ispell-buffer)
  :init
  (setq ispell-dictionary "en_US")
  ;; http://stackoverflow.com/questions/2376113/personal-dictionaries-in-emacs-flyspell-mode
  (setq ispell-personal-dictionary "~/.emacs.d/misc/aspell.en.pws")
  :config

  ;; Ignore Japanese
  ;; http://keisanbutsuriya.blog.fc2.com/blog-entry-60.html
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  ;; Use i in ispell minor mode to save the word
  ;; http://stackoverflow.com/questions/11070849/flyspell-without-a-mouse
  (defun save-ispell-word (word)
    (interactive "sA word you want to add to dictionary ")
    (ispell-send-string (concat "*" word "\n"))
    (setq ispell-pdict-modified-p '(t))))


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
