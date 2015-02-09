;;; flyspell-mode related

;;;
;;; On Macx, configure aspell
;; Mac-only configuration
(when (eq system-type 'darwin)
  ;; http://d.hatena.ne.jp/yutoichinohe/20140120/1390225624
  ;; brew install aspell --with-lang-en
  (setq ispell-program-name "/usr/local/bin/aspell"))

;;; Configuration
;; http://keisanbutsuriya.blog.fc2.com/blog-entry-60.html


;;;
;;; ispell.el (built-in)
;; Ignore Japanese
;; http://keisanbutsuriya.blog.fc2.com/blog-entry-60.html
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;;
(setq ispell-dictionary "en_US")
;;
(global-set-key (kbd "s-c") 'ispell-word)
;;
(require 'ispell)
;;
;; Use i in ispell minor mode to save the word
;; http://stackoverflow.com/questions/11070849/flyspell-without-a-mouse
(defun save-ispell-word (word)
  (interactive "sA word you want to add to dictionary ")
  (ispell-send-string (concat "*" word "\n"))
  (setq ispell-pdict-modified-p '(t)))


;;;
;;; flyspell.el (built-in)
(require 'flyspell)


;;;
;;; Define a fuction to use the popup.el
;; 2015-02-09 Currently not functional
;; http://d.hatena.ne.jp/mooz/20100423/p1
(defun flyspell-correct-word-popup-el ()
  "Pop up a menu of possible corrections for misspelled word before point."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
	(word (flyspell-get-word nil)))
    (if (consp word)
	(let ((start (car (cdr word)))
	      (end (car (cdr (cdr word))))
	      (word (car word))
	      poss ispell-filter)
	  ;; now check spelling of word.
	  (ispell-send-string "%\n")	;put in verbose mode
	  (ispell-send-string (concat "^" word "\n"))
	  ;; wait until ispell has processed word
	  (while (progn
		   (accept-process-output ispell-process)
		   (not (string= "" (car ispell-filter)))))
	  ;; Remove leading empty element
	  (setq ispell-filter (cdr ispell-filter))
	  ;; ispell process should return something after word is sent.
	  ;; Tag word as valid (i.e., skip) otherwise
	  (or ispell-filter
	      (setq ispell-filter '(*)))
	  (if (consp ispell-filter)
	      (setq poss (ispell-parse-output (car ispell-filter))))
	  (cond
	   ((or (eq poss t) (stringp poss))
	    ;; don't correct word
	    t)
	   ((null poss)
	    ;; ispell error
	    (error "Ispell: error in Ispell process"))
	   (t
	    ;; The word is incorrect, we have to propose a replacement.
	    (flyspell-do-correct (popup-menu* (car (cddr poss)) :scroll-bar t :margin t)
				 poss word cursor-location start end cursor-location)))
	  (ispell-pdict-save t)))))
;;
;; (global-set-key (kbd "s-c") 'flyspell-correct-word-popup-el)


;;;
;;; Auto-start flyspell-mode for these files
;; Not on, it can be annoying in Japanese input
;; (add-to-list 'auto-mode-alist '("\\.txt" . flyspell-mode))
;; (add-to-list 'auto-mode-alist '("\\.tex" . flyspell-mode))


;;;
;;; ac-ispell.el
;; Auto-completion for English words
(custom-set-variables
  '(ac-ispell-requires 4))
;;
(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))
;;
(defun my/enable-ac-ispell ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ispell))
;;
;; Enable for these modes
(add-hook 'git-commit-mode-hook 'my/enable-ac-ispell)
(add-hook 'mail-mode-hook 'my/enable-ac-ispell)
(add-hook 'text-mode-hook 'my/enable-ac-ispell)
