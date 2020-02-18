;;; spell check related

;;;
;;; ispell.el
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
;;
;; Hunspell English Dictionaries
;; http://wordlist.aspell.net/hunspell-readme/
;; SCOWL (Spell Checker Oriented Word Lists) and Friends
;; http://wordlist.aspell.net
;; http://wordlist.aspell.net/dicts/
(use-package ispell
  :commands (ispell-word
             ispell-region
             ispell-buffer)
  :config
  ;; Use best program available
  (let ((hunspell-file (executable-find "hunspell"))
        (aspell-file (executable-find "aspell"))
        (ispell-file (executable-find "ispell"))
        (mac-dict-paths '(("en_US" "~/Library/Spelling/en_US.aff")
                          ("en_US-med" "~/Library/Spelling/en_US-med.aff"))))
    (cond
     ;; hunspell
     ;; hunspell -D to check paths. /Library/Spelling/ was added for use.
     (hunspell-file
      (setq ispell-really-hunspell hunspell-file)
      (setq ispell-program-name hunspell-file)
      ;; http://emacs.stackexchange.com/questions/21378/spell-check-with-multiple-dictionaries
      ;; http://gromnitsky.blogspot.com/2016/09/emacs-251-hunspell.html
      ;; This is then looked for in the alist below.
      (setq ispell-dictionary "en_US")
      ;; This is the real configuration.
      (when (eq system-type 'darwin)
        (setq ispell-dictionary-alist mac-dict-paths)
        (setq ispell-hunspell-dict-paths-alist mac-dict-paths))
      (setq ispell-personal-dictionary (concat user-emacs-directory
                                               "misc/hunspell.en")))
     ;; aspell
     ;; install with brew install aspell --with-lang-en
     (aspell-file
      (setq ispell-program-name aspell-file)
      (setq ispell-dictionary "en_US")
      (setq ispell-personal-dictionary (concat user-emacs-directory
                                               "misc/aspell.en.pws")))
     ;; ispell
     (ispell-file
      (setq ispell-program-name ispell-file)
      (when (eq system-type 'darwin)
        (setq ispell-dictionary-alist mac-dict-paths))
      (setq ispell-personal-dictionary (concat user-emacs-directory
                                               "misc/ispell.en")))))
  ;;
  ;; http://stackoverflow.com/questions/2376113/personal-dictionaries-in-emacs-flyspell-mode
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
;;; flyspell.el
;;
(use-package flyspell
  :disabled t
  :diminish flyspell-mode
  :commands (flyspell-goto-next-error
             flyspell-mode
             turn-on-flyspell
             turn-off-flyspell
             flyspell-prog-mode
             flyspell-check-goto-next-error-and-popup
             flyspell-goto-next-error-and-popup)
  :init
  ;; Activate for some modes.
  (dolist (mode-hook '(text-mode-hook
                       git-commit-mode-hook))
    (add-hook mode-hook #'turn-on-flyspell))
  ;;
  ;; Use the comment-only spell check in programming modes.
  ;; https://joelkuiper.eu/spellcheck_emacs
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       inferior-lisp-mode-hook
                       clojure-mode-hook
                       python-mode-hook
                       js-mode-hook
                       R-mode-hook))
    (add-hook mode-hook #'flyspell-prog-mode))
  ;;
  :config
  ;; Unset some key bindings to avoid collisions
  (setq flyspell-auto-correct-binding nil)
  ;;
  (defun flyspell-check-goto-next-error-and-popup ()
    "Fly-spell the current buffer, go to the next error, and pop up a menu."
    (interactive)
    (flyspell-buffer)
    (flyspell-goto-next-error)
    (flyspell-popup-correct))
  (defun flyspell-goto-next-error-and-popup ()
    "Go to the next error and pop up a menu."
    (interactive)
    (flyspell-goto-next-error)
    (flyspell-popup-correct))
  ;;
  (eval-after-load "flyspell"
    #'(lambda ()
        ;; Drop unnecessary key bindings.
        ;; http://stackoverflow.com/questions/16084022/emacs-flyspell-deactivate-c-key-binding
        (define-key flyspell-mode-map (kbd "C-,") nil)
        (define-key flyspell-mode-map (kbd "C-.") nil)
        (define-key flyspell-mode-map (kbd "C-;") nil)
        (define-key flyspell-mode-map (kbd "C-c $") nil)
        (define-key flyspell-mode-map (kbd "C-M-i") nil)
        ;; Bind commands to less problematic keys.
        (define-key flyspell-mode-map (kbd "A-,") 'flyspell-check-goto-next-error-and-popup)
        (define-key flyspell-mode-map (kbd "A-.") 'flyspell-goto-next-error-and-popup))))
;;
;;
;;;  flyspell-popup.el
;; https://github.com/xuchunyang/flyspell-popup
;; https://github.com/d12frosted/flyspell-correct
(use-package flyspell-popup
  :commands (flyspell-popup-correct)
  :bind ("s-c" . flyspell-popup-correct)
  :config
  ;; (global-set-key (kbd "A-m") '(lambda ()
  ;;                                (interaction)
  ;;                                (mouse--down-1-maybe-follows-link)))
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
              #'turn-on-flyspell))


;;;
;;; langtools.el
;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool
  :commands (langtool-check)
  :config
  ;; The standalone desktop version was downloaded from the following URL.
  ;; https://www.languagetool.org
  (setq langtool-language-tool-jar (concat user-emacs-directory
                                           "misc/LanguageTool-3.5/languagetool-commandline.jar"))
  ;; Language name pass to LanguageTool.
  (setq langtool-default-language "en-US")
  ;; Your mothertongue Language name pass to LanguageTool.
  (setq langtool-mother-tongue nil)
  ;; Disabled rules pass to LanguageTool.
  ;; String that separated by comma or list of string.
  (setq langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES"))
  ;; Keys
  (global-set-key (kbd "A-/") 'langtool-goto-next-error)
  ;; Show LanguageTool report automatically by popup.
  ;; This idea come from: http://d.hatena.ne.jp/LaclefYoshi/20150912/langtool_popup
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup))



;;;
;;; osx-dictionary.el
;; https://github.com/xuchunyang/osx-dictionary.el
(use-package osx-dictionary
  :commands (osx-dictionary-search-word-at-point
             osx-dictionary-search-input))


;;;
;;; synonymous.el
;; https://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; https://www.emacswiki.org/emacs/synonyms.el
(use-package synonymous
  :commands (synonymous-synonyms))


;;;
;;; synosaurus.el
;; https://github.com/hpdeifel/synosaurus
(use-package synosaurus
  :commands (synosaurus-mode
             synosaurus-lookup
             synosaurus-choose-and-replace)
  ;; :bind ("s-x" . synosaurus-choose-and-replace)
  ;;
  :config
  (use-package synosaurus-wordnet
    :commands (synosaurus-backend-wordnet))
  ;; synosaurus-backend-wordnet        An english offline thesaurus
  ;; https://wordnet.princeton.edu
  ;; $ brew install wordnet # for wn command.
  (setq synosaurus-backend 'synosaurus-backend-wordnet)
  ;;
  ;; The way of querying the user for word replacements.
  (setq synosaurus-choose-method 'popup))


;;;
;;; flycheck-grammarly.el
;; https://github.com/jcs-elpa/flycheck-grammarly/tree/master
(use-package flycheck-grammarly
  :config
  ;; The following is done in the file
  ;; (add-to-list 'flycheck-checkers 'grammarly-checker)
  )
