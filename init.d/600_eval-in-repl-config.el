;;; eval-in-repl configuration

;;; Uncomment to use the local dev repo
(let ((eir-repo "~/Documents/programming/emacs-lisp-repos/eval-in-repl"))
  (when (file-exists-p eir-repo)
    (add-to-list 'load-path eir-repo)))


;;; require the main file containing common functions
(use-package eval-in-repl)


;;; Default behaviors
;; Always delete other windows
(setq eir-delete-other-windows nil)
;; Always split
(setq eir-always-split-script-window nil)
;; Place REPL on the left when splitting
(setq eir-repl-placement 'left)

;;; ielm support (for emacs lisp)
(use-package eval-in-repl-ielm
  :commands eir-eval-in-ielm)
;; for .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; for *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; for M-x info
(define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)


;;; cider support (for Clojure)
;; (require 'cider) ; if not done elsewhere
(use-package eval-in-repl-cider
  :commands eir-eval-in-cider)
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider))


;;; SLIME support (for common lisp)
;; (require 'slime) ; if not done elsewhere
(use-package eval-in-repl-slime
         :commands eir-eval-in-slime)
;; (with-eval-after-load 'slime
;;   (define-key lisp-mode-map (kbd "<C-return>") 'eir-eval-in-slime))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))


;;; geiser support (for Racket and Guile Scheme)
;; When using this, turn off racket-mode and scheme supports
;; (require 'geiser) ; if not done elsewhere
(use-package eval-in-repl-geiser
  :commands eir-eval-in-geiser)
(add-hook 'geiser-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))

;; ;;; racket-mode support (for Racket)
;; (require 'racket-mode) ; if not done elsewhere
;; (require 'eval-in-repl-racket)
;; (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

;; ;;; scheme support
;; (require 'scheme)    ; if not done elsewhere
;; (require 'cmuscheme) ; if not done elsewhere
;; (require 'eval-in-repl-scheme)
;; (add-hook 'scheme-mode-hook
;; 	  '(lambda ()
;; 	     (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))


;;; python support
;; (require 'python) ; if not done elsewhere
(use-package eval-in-repl-python
  :commands eir-eval-in-python)
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
(add-hook 'ein:notebook-multilang-mode
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
;; (define-key ein:notebook-mode-map (kbd "<C-return>") 'eir-eval-in-python)


;;; shell support
;; (require 'essh) ; if not done elsewhere
(use-package eval-in-repl-shell
  :commands eir-eval-in-shell
  :config
  ;; Version with opposite behavior to eir-jump-after-eval configuration
  (defun eir-eval-in-shell2 ()
    "eval-in-repl for shell script (opposite behavior)

This version has the opposite behavior to the eir-jump-after-eval
configuration when invoked to evaluate a line."
    (interactive)
    (let ((eir-jump-after-eval (not eir-jump-after-eval)))
      (eir-eval-in-shell)))
  (add-hook 'sh-mode-hook
            '(lambda()
               (local-set-key (kbd "C-M-<return>") 'eir-eval-in-shell2))))
;;
(add-hook 'sh-mode-hook
          '(lambda()
	     (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))


;;; sml support
;; (require 'sml-mode) ; if not done elsewhere
(use-package eval-in-repl-sml
  :commands eir-eval-in-sml)
;;
(with-eval-after-load 'sml-mode
  (define-key sml-mode-map (kbd "<C-return>") 'eir-eval-in-sml)
  ;; function to send a semicolon to SML REPL
  (define-key sml-mode-map (kbd "C-;") 'eir-send-to-sml-semicolon))



;;; ruby support
;; (require 'ruby-mode) ; if not done elsewhere
;; (require 'inf-ruby)  ; if not done elsewhere
;; (require 'ess)       ; if not done elsewhere
(use-package eval-in-repl-ruby
  :commands eir-eval-in-ruby)
;;
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby))


;;; ocaml support
;; (require 'tuareg) ; if not done elsewhere
(use-package eval-in-repl-ocaml
  :commands eir-eval-in-ocaml)
;;
(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map (kbd "<C-return>") 'eir-eval-in-ocaml)
  ;; function to send a semicolon to OCaml REPL
  (define-key tuareg-mode-map (kbd "C-;") 'eir-send-to-ocaml-semicolon))


;;; hy support
;; (require 'hy-mode) ; if not done elsewhere
(use-package eval-in-repl-hy
  :commands eir-eval-in-hy)
;;
(with-eval-after-load 'hy-mode
  (define-key hy-mode-map (kbd "<C-return>") 'eir-eval-in-hy))


;;; javascript support
;; (require 'js-comint) ; if not done elsewhere
(use-package eval-in-repl-javascript
  :commands eir-eval-in-javascript)
;;
(with-eval-after-load 'js3-mode
  (require 'eval-in-repl-javascript)
  (define-key js3-mode-map (kbd "<C-return>") 'eir-eval-in-javascript))

(with-eval-after-load 'js2-mode
  (require 'eval-in-repl-javascript)
  (define-key js2-mode-map (kbd "<C-return>") 'eir-eval-in-javascript))
