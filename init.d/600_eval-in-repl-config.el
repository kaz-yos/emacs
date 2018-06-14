;;; 600_eval-in-repl-config.el ---                   -*- lexical-binding: t; -*-


;;; EVAL-IN-REPL-RELATED
;;;  Uncomment to use the local dev repo
;; (let ((eir-repo "~/Documents/programming/emacs-lisp-repos/eval-in-repl"))
;;   (when (file-exists-p eir-repo)
;;     (add-to-list 'load-path eir-repo)))


;;;  eval-in-repl.el
;; require the main file containing common functions
(use-package eval-in-repl
  :config
  ;;; Default behaviors
  ;; Always delete other windows
  (setq eir-delete-other-windows nil)
  ;; Always split
  (setq eir-always-split-script-window nil)
  ;; Place REPL on the left when splitting
  (setq eir-repl-placement 'left))


;;;  ielm support (for emacs lisp)
(use-package eval-in-repl-ielm
  :commands (eir-eval-in-ielm)
  :bind (;; for .el files
         :map emacs-lisp-mode-map
         ("C-<return>" . eir-eval-in-ielm)
         ;; for *scratch*
         :map lisp-interaction-mode-map
         ("C-<return>" . eir-eval-in-ielm)
         ;; for M-x info
         :map Info-mode-map
         ("C-<return>" . eir-eval-in-ielm))
  :config
  (setq eir-ielm-eval-in-current-buffer t))


;;;  cider support (for Clojure)
(use-package eval-in-repl-cider
  :after cider
  :commands (eir-eval-in-cider)
  :config
  (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider))


;;;  SLIME support (for common lisp)
(use-package eval-in-repl-slime
  :after slime
  :commands (eir-eval-in-slime)
  :config
  (add-hook 'lisp-mode-hook
            '(lambda ()
               (local-set-key (kbd "<C-return>") 'eir-eval-in-slime))))


;;;  geiser support (for Racket and Guile Scheme)
;; When using this, turn off racket-mode and scheme supports
;; (require 'geiser) ; if not done elsewhere
(use-package eval-in-repl-geiser
  :after geiser-mode
  :commands (eir-eval-in-geiser)
  :config
  (add-hook 'geiser-mode-hook
            '(lambda ()
               (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser))))


;;;  racket-mode support (for Racket)
(use-package eval-in-repl-racket
  :after racket-mode
  :commands (eir-eval-in-racket)
  :bind (:map racket-mode-map
              ("C-<return>" . eir-eval-in-racket)))


;;;  scheme support
(use-package eval-in-repl-scheme
  :after (scheme cmuscheme)
  :commands (eir-eval-in-racket)
  :hook (scheme-mode . (lambda ()
                         (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme))))


;;;  python support
;; (require 'python) ; if not done elsewhere
(use-package eval-in-repl-python
  :commands (eir-eval-in-python)
  :hook (python-mode . (lambda ()
                         (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))


;;;  shell support
(use-package eval-in-repl-shell
  :after sh-mode
  :commands eir-eval-in-shell
  :bind (:map sh-mode-map
              ("C-<return>" . eir-eval-in-shell))
  :config
  ;; Version with opposite behavior to eir-jump-after-eval configuration
  (defun eir-eval-in-shell2 ()
    "eval-in-repl for shell script (opposite behavior)

This version has the opposite behavior to the eir-jump-after-eval
configuration when invoked to evaluate a line."
    (interactive)
    (let ((eir-jump-after-eval (not eir-jump-after-eval)))
      (eir-eval-in-shell))))


;;;  sml support
;; (require 'sml-mode) ; if not done elsewhere
(use-package eval-in-repl-sml
  :after sml-mode
  :commands eir-eval-in-sml
  :bind (:map sml-mode-map
              ("<C-return>" . eir-eval-in-sml)
              ("C-;" . eir-send-to-sml-semicolon)))


;;;  ruby support
;; (require 'ruby-mode) ; if not done elsewhere
;; (require 'inf-ruby)  ; if not done elsewhere
(use-package eval-in-repl-ruby
  :after ruby-mode
  :commands (eir-eval-in-ruby)
  :bind (:map ruby-mode-map
              ("<C-return>" . eir-eval-in-ruby)))


;;;  ocaml support
(use-package eval-in-repl-ocaml
  :after tuareg
  :commands (eir-eval-in-ocaml)
  :bind (:map sml-mode-map
              ("<C-return>" . eir-eval-in-ocaml)
              ("C-;" . eir-send-to-ocaml-semicolon)))


;;;  hy support
;; (require 'hy-mode) ; if not done elsewhere
(use-package eval-in-repl-hy
  :after hy-mode
  :commands (eir-eval-in-hy)
  :bind (:map hy-mode-map
              ("<C-return>" . eir-eval-in-hy)))


;;;  javascript support
(use-package eval-in-repl-javascript
  :after js3-mode
  :commands eir-eval-in-javascript
  :bind (:map js3-mode-map
              ("<C-return>" . eir-eval-in-javascript)
              :map js2-mode-map
              ("<C-return>" . eir-eval-in-javascript)))


;;;  octave support
(use-package eval-in-repl-octave
  :load-path "~/Documents/programming/emacs-lisp-repos/eval-in-repl"
  :after octave
  :commands eir-eval-in-octave
  :bind (:map octave-mode-map
              ("<C-return>" . eir-eval-in-octave)))


;;;
;;; org-babel-eval-in-repl.el
;; https://github.com/diadochos/org-babel-eval-in-repl
(use-package org-babel-eval-in-repl
  :after ob
  :bind (:map org-mode-map
              ("C-<return>" . ober-eval-in-repl)))
