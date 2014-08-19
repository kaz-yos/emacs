;;; eval-in-repl configuration

;; require the main file containing common functions
(require 'eval-in-repl)

;; ielm
(require 'eval-in-repl-ielm)
;; For .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; For *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; For M-x info
(define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

;; cider
;; (require 'cider) ; if not done elsewhere
(require 'eval-in-repl-cider)
(define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)

;; Racket
;; (require 'racket-mode) ; if not done elsewhere
;; (require 'eval-in-repl-racket)
;; (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

;; SLIME
;; (require 'slime) ; if not done elsewhere
(require 'eval-in-repl-slime)
(add-hook 'lisp-mode-hook
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))

;; scheme
;; (require 'scheme) ; if not done elsewhere
;; (require 'cmuscheme) ; if not done elsewhere
(require 'eval-in-repl-scheme)
(add-hook 'scheme-mode-hook
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))

;; python
;; (require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)

;; shell
;; (require 'essh) ; if not done elsewhere
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
		     (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))
