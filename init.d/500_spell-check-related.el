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
  :config
  ;; Unset some key bindings to avoid collisions
  (setq flyspell-auto-correct-binding nil)
  ;;
  (setq flyspell-mode-map (make-sparse-keymap))
  (define-key flyspell-mode-map (kbd "A-,") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "A-.") 'flyspell-popup-correct)
  ;;
  ;; Auto-start flyspell-mode for these files
  ;; 2015-02-09 AquaSKK appears ok.
  ;; (add-to-list 'auto-mode-alist '("\\.txt" . flyspell-mode))
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
  (advice-add 'flyspell-popup-correct
              :after
              #'turn-off-flyspell))


;; ;;;
;; ;;; ac-ispell.el
;; ;; Auto-completion for English words
;; (setq ac-ispell-requires 4)
;; ;;
;; (eval-after-load "auto-complete"
;;   '(progn
;;       (ac-ispell-setup)))
;; ;;
;; (defun my/enable-ac-ispell ()
;;   (interactive)
;;   (add-to-list 'ac-sources 'ac-source-ispell)
;;   (auto-complete-mode 1))
;; ;;
;; ;; Enable for these modes
;; (add-hook 'git-commit-mode-hook 'my/enable-ac-ispell)
;; (add-hook 'mail-mode-hook 'my/enable-ac-ispell)
;; ;; 2015-02-09 Not functional?
;; (add-hook 'fundamental-mode-hook 'auto-complete-mode)
;; (add-hook 'fundamental-mode-hook 'my/enable-ac-ispell)
