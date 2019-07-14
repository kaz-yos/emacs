;;; 500_stan-related.el ---                          -*- lexical-binding: t; -*-

;;;
;;; stan-mode.el
(use-package stan-mode
  :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :commands (my-stan-setup
             company-stan-backend)
  :hook (stan-mode . my-stan-setup)
  ;;
  :config
  ;; These variables are not directly used.
  (setq stan-comment-start "//")
  (setq stan-comment-end "")
  ;; Rather, they have to be set to buffer-local comment-start/end variables.
  (defun my-stan-setup ()
    (setq comment-start stan-comment-start)
    (setq comment-end stan-comment-end)
    ;; Two-character indent.
    ;; https://mc-stan.org/docs/2_18/stan-users-guide/white-space.html
    (setq c-basic-offset 2)
    ;; Add company-stan-backend to the company-backends buffer-locally.
    (add-to-list (make-local-variable 'company-backends)
                 'company-stan-backend))
  ;;
  ;; Writing backends for the company-mode.
  ;; https://github.com/company-mode/company-mode/wiki/Writing-backends
  ;; http://sixty-north.com/blog/series/how-to-write-company-mode-backends.html
  ;; Definitions
  ;; https://github.com/company-mode/company-mode/blob/master/company.el
  ;; Example in company-math.el
  ;; https://github.com/vspinu/company-math/blob/master/company-math.el#L210
  ;;
  ;; stan-keywords-lists.el defines the following:
  ;;  stan-types-list
  ;;  stan-function-return-types-list
  ;;  stan-blocks-list
  ;;  stan-range-constraints-list
  ;;  stan-keywords-list
  ;;  stan-functions-list
  ;;  stan-distribution-list
  ;;  stan-reserved-list
  ;;  Do NOT USE stan-deprecated-function-list
  ;;
  (defun propertize-list (lst category)
    "Propertize each element of a string list"
    (mapcar (lambda (s)
              (propertize s :category category))
            lst))
  ;;
  (defun company-stan-backend-annotation (s)
    "Construct an annotation string from the :category property"
    (format " [%s]" (get-text-property 0 :category s)))
  ;;
  ;; The signature (command &optional arg &rest ignored) is mandated.
  (defun company-stan-backend (command &optional arg &rest ignored)
    "company-mode backend function for stan-mode"
    ;; Making it interactive allows interactive testing.
    (interactive (list 'interactive))
    ;; (cl-case EXPR (KEYLIST BODY...)...)
    ;; Eval EXPR and choose among clauses on that value.
    ;; Here we decide what to do based on COMMAND.
    ;; One of {interactive, prefix, candidates, annotation}
    (cl-case command
      ;; 1. interactive call
      ;; (company-begin-backend BACKEND &optional CALLBACK)
      ;; Start a completion at point using BACKEND.
      (interactive (company-begin-backend 'company-stan-backend))
      ;; 2. prefix command
      ;;  It should return the text that is to be completed.
      ;;  If it returns nil, this backend is not used.
      ;;  Here we need to verify the major mode.
      (prefix (and (eq major-mode 'stan-mode)
                   ;; If point is at the end of a symbol, return it for completion.
                   ;; Otherwise, if point is not inside a symbol, return an empty string.
                   ;; This will give the prefix to be completed.
                   (company-grab-symbol)))
      ;; 3. candidates command
      ;;  This is where we actually generate a list of possible completions.
      ;;  When this is called arg holds the prefix string to be completed
      (candidates
       (cl-remove-if-not
        ;; Retain if matching
        (lambda (c) (my-company-fuzzy-match arg c))
        ;; from a long list of all stan object names.
        (append (propertize-list stan-types-list "type")
                (propertize-list stan-function-return-types-list "function return type")
                (propertize-list stan-blocks-list "block")
                (propertize-list stan-range-constraints-list "range constraint")
                (propertize-list stan-keywords-list "keyword")
                (propertize-list stan-functions-list "function")
                (propertize-list stan-distribution-list "distribution")
                (propertize-list stan-reserved-list "reserved")
                )))
      ;; 4. annotation command
      (annotation (company-stan-backend-annotation arg))))
  )


;;; eldoc-stan.el
(use-package eldoc-stan
  :load-path "~/Documents/programming/emacs-lisp-repos/stan-mode/eldoc-stan/"
  :hook (stan-mode . eldoc-stan-setup)
  )
