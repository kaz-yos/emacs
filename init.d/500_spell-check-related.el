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
        ;; Bind to less problematics ones
        (define-key flyspell-mode-map (kbd "A-,") 'flyspell-goto-next-error)
        (define-key flyspell-mode-map (kbd "A-.") 'flyspell-popup-correct)))
  ;; ;; Reset to an empty map
  ;; (setq flyspell-mode-map (make-sparse-keymap))
  ;; ;;
  ;; (define-key global-map (kbd "A-,") 'flyspell-goto-next-error)
  ;; (define-key global-map (kbd "A-.") 'flyspell-popup-correct)
  )
;;
;;
;;; flyspell-popup.el
;; https://github.com/xuchunyang/flyspell-popup
(use-package flyspell-popup
  :commands (flyspell-popup-correct)
  :bind ("s-c" . flyspell-popup-correct)
  :config
  ;; Turn flyspell-mode on before invoking popup
  (advice-add 'flyspell-popup-correct
              :before
              #'turn-on-flyspell)
  ;; Turn flyspell-mode off after invoking popup
  (advice-add 'flyspell-popup-correc
              :after
              #'turn-off-flyspell))
