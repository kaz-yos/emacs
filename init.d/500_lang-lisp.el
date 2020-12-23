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
  :commands (enable-lispy)
  ;; 2020-05-11 Interfered with Shelly's arrow keys.
  ;; :hook ((emacs-lisp-mode . enable-lispy)
  ;;        (minibuffer-setup . enable-lispy-conditionally))
  :config
  ;; https://github.com/abo-abo/lispy#configuration-instructions
  (defun enable-lispy ()
    (lispy-mode 1))
  ;; Enable lispy for eval-expression (M-:)
  (defun enable-lispy-conditionally ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1))))


;;;
;;; lisp-mode.el
(use-package lisp-mode
  :config
  ;; How to indent keywords aligned?
  ;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
  ;; Redefines the silly indent of keyword lists
  ;; before
  ;;   (:foo bar
  ;;         :baz qux)
  ;; after
  ;;   (:foo bar
  ;;    :baz qux)
  (defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.

This is a modified version by Fuco1.
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned

The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state)))))))))


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
