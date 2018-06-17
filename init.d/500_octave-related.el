;;; 500_octave-related.el ---                        -*- lexical-binding: t; -*-

;;;
;;; octave.el
;; https://octave.org/doc/v4.0.0/Using-Octave-Mode.html
(use-package octave
  :mode ("\\.m\\'" . octave-mode)
  ;; https://emacs.stackexchange.com/questions/10713/how-to-change-comment-character-from-hash-to-percent-sign-for-octave-maj
  :hook ((octave-mode . (lambda () (setq comment-start "% ")))))
