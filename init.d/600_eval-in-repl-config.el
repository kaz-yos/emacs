;;; eval-in-repl configuration

;; Uncomment to use the local dev repo
(add-to-list 'load-path "~/Documents/programming/emacs-lisp-repos/eval-in-repl")

;; require the main file containing common functions
(require 'eval-in-repl)

;; ielm support (for emacs lisp)
(require 'eval-in-repl-ielm)
;; for .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; for *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; for M-x info
(define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

;; cider support (for Clojure)
;; (require 'cider) ; if not done elsewhere
(require 'eval-in-repl-cider)
(define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)

;; SLIME support (for common lisp)
;; (require 'slime) ; if not done elsewhere
(require 'eval-in-repl-slime)
(add-hook 'lisp-mode-hook
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))

;; geiser support (for Racket and Guile Scheme)
;; When using this, turn off racket-mode and scheme supports
;; (require 'geiser) ; if not done elsewhere
(require 'eval-in-repl-geiser)
(add-hook 'geiser-mode-hook
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))

;; ;; racket-mode support (for Racket)
;; (require 'racket-mode) ; if not done elsewhere
;; (require 'eval-in-repl-racket)
;; (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

;; ;; scheme support
;; (require 'scheme)    ; if not done elsewhere
;; (require 'cmuscheme) ; if not done elsewhere
;; (require 'eval-in-repl-scheme)
;; (add-hook 'scheme-mode-hook
;; 	  '(lambda ()
;; 	     (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))

;; python support
;; (require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)
(add-hook 'ein:notebook-multilang-mode
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
;; (define-key ein:notebook-mode-map (kbd "<C-return>") 'eir-eval-in-python)

;; shell
;; (require 'essh) ; if not done elsewhere
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
	     (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

;; sml support
;; (require 'sml-mode) ; if not done elsewhere
(require 'eval-in-repl-sml)
(define-key sml-mode-map (kbd "<C-return>") 'eir-eval-in-sml)
;; function to send a semicolon to SML REPL
(define-key sml-mode-map (kbd "C-;") 'eir-send-to-sml-semicolon)

;; ruby support
;; (require 'ruby-mode) ; if not done elsewhere
;; (require 'inf-ruby)  ; if not done elsewhere
;; (require 'ess)       ; if not done elsewhere
(require 'eval-in-repl-ruby)
(define-key ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby)

;; ocaml support
;; (require 'tuareg) ; if not done elsewhere
(require 'eval-in-repl-ocaml)
(define-key tuareg-mode-map (kbd "<C-return>") 'eir-eval-in-ocaml)
;; function to send a semicolon to OCaml REPL
(define-key tuareg-mode-map (kbd "C-;") 'eir-send-to-ocaml-semicolon)

;; hy support
;; (require 'hy-mode) ; if not done elsewhere
(require 'eval-in-repl-hy)
(define-key hy-mode-map (kbd "<C-return>") 'eir-eval-in-hy)
