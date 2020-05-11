;;; -*- lexical-binding: t; -*-

;;;
;;; paredit.el
;; smartparens appears more modern. 2014-02-03
;; https://github.com/Fuco1/smartparens
;;
;; M-x install-elisp http://mumble.net/~campbell/emacs/paredit.el
(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (clojure-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (hy-mode . enable-paredit-mode))
  :config
  (setq paredit-lighter "")
  ;; No space when inserted after a word
  ;; http://stackoverflow.com/questions/913449/changing-paredit-formatting
  (defun paredit-space-for-delimiter-p (endp delimiter)
    (and (not (if endp (eobp) (bobp)))
         (memq (char-syntax (if endp (char-after) (char-before)))
               (list ?\"  ;; REMOVED ?w ?_
                     (let ((matching (matching-paren delimiter)))
                       (and matching (char-syntax matching))))))))

;;;
;;; lispy.el
;; https://github.com/abo-abo/lispy
(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode . enable-lispy)
         (minibuffer-setup . enable-lispy-conditionally))
  :config
  ;; https://github.com/abo-abo/lispy#configuration-instructions
  (defun enable-lispy ()
    (lispy-mode 1))
  ;; Enable lispy for eval-expression (M-:)
  (defun enable-lispy-conditionally ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1))))



;;;
;;; SLIME for non-elisp lisps

;;;  slime-company.el
;; https://github.com/anwyn/slime-company
(use-package slime-company
  :ensure t
  :commands (slime-company))

;;;  slime.el
;;
;; http://www.common-lisp.net/project/slime/
;; http://dev.ariel-networks.com/wp/archives/462
(use-package slime
  :ensure t
  :commands (slime)
  ;;
  :config
  ;; Setup Emacs so that lisp-mode buffers always use SLIME.
  (slime-setup '(slime-repl slime-fancy slime-banner slime-company))
  ;;
  ;; Common Lisp hyperspec via homebrew
  ;; http://www.lispworks.com/documentation/common-lisp.html
  (setq common-lisp-hyperspec-root
        "/usr/local/share/doc/hyperspec/HyperSpec/")
  (setq common-lisp-hyperspec-symbol-table
        (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (setq common-lisp-hyperspec-issuex-table
        (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))
  ;;
  ;; 2.5.2 Multiple Lisps (The first one serves as the default.)
  ;; http://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html
  ;; (NAME (PROGRAM PROGRAM-ARGS...) &key CODING-SYSTEM INIT INIT-FUNCTION ENV)
  ;; NAME is a symbol and is used to identify the program.
  ;; PROGRAM is the filename of the program. Note that the filename can contain spaces.
  ;; PROGRAM-ARGS is a list of command line arguments.
  ;; CODING-SYSTEM the coding system for the connection. (see slime-net-coding-system)
  (setq slime-lisp-implementations
        '((clisp  ("/usr/local/bin/clisp"))
          (sbcl   ("/usr/local/bin/sbcl")))))


;;;
;;; RACKET RELATED
;; Racket documentation for emacs
;; http://docs.racket-lang.org/guide/Emacs.html
;;
;; Package Management in Racket
;; http://docs.racket-lang.org/pkg/
;;
;; Install these to make racket-mode work
;; $ raco pkg install rackunit

;;;  racket-mode.el
;; https://github.com/greghendershott/racket-mode
;; major mode for Racket. incompatible with geiser minor mode.
;;
;; (require 'racket-mode)

;;;  geiser.el
;; Geiser for Racket and Guile Scheme
;; Works as an add-on to the built-in scheme mode
;; http://www.nongnu.org/geiser/
;;
;; Do $ raco pkg install compatibility-lib if installing plt-racket via Homebrew
;; https://github.com/jaor/geiser/issues/39
(use-package geiser
  :ensure t
  :commands (geiser-mode switch-to-geiser)
  :config
;;;  ac-geiser.el
  (when (require 'ac-geiser nil 'noerror)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'geiser-repl-mode))
    ;;
    (defun my-ac-geiser-setup ()
      (ac-geiser-setup)
      (company-mode -1)
      (define-key geiser-mode-map (kbd "C-.") 'highlight-symbol-at-point))
    (add-hook 'geiser-mode-hook      'my-ac-geiser-setup)
    (add-hook 'geiser-repl-mode-hook 'my-ac-geiser-setup)))
