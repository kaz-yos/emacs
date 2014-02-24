;;; SLIME
;; http://www.common-lisp.net/project/slime/
;; http://dev.ariel-networks.com/wp/archives/462
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
;;
;; elisp as inferior-lisp-program	; did not work
;; http://stackoverflow.com/questions/6687721/repl-for-emacs-lisp
;;(setq inferior-lisp-program "/usr/local/bin/emacs --batch --eval '(while t (print (eval (read))))'")
;;
;; Common lisp (installed via homebrew)
(setq inferior-lisp-program "/usr/local/bin/clisp")
