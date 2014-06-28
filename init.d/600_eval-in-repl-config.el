;;; eval-in-repl configuration

;; Set load-path
(add-to-list 'load-path "~/Documents/programming/emacs-lisp-repos/eval-in-repl")

;; require the skeleton package
(require 'eval-in-repl)

;; cider
(require 'eval-in-repl-cider)
(define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)

;; ielm
(require 'eval-in-repl-ielm)
;; For .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; For *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; For M-x info
(define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

;; SLIME
(require 'eval-in-repl-slime)
(add-hook 'lisp-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))

;; scheme-mode
(require 'eval-in-repl-scheme)
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))

;; python
(require 'eval-in-repl-python)
(define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)

;; shell
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
	     (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))
