;;;
;;; tree-sitter
;; https://tree-sitter.github.io/tree-sitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter


;;;
;;; tree-sit.el
(use-package treesit
  ;; Available in emacs 29 and later.
  :if (not (version< emacs-version "29.0"))
  :demand t
  ;;
  :config
  ;; Use M-x treesit-install-language-grammar
  (setq treesit-language-source-alist
        ;; Check each language with (treesit-language-available-p 'bash) etc
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
