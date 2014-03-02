;;; dired-plus 2014-02-04
;; http://www.emacswiki.org/emacs/DiredPlus
;; http://ergoemacs.org/emacs/emacs_diredplus_mode.html
(require 'dired+)
;;
;;  Hide/Show Details
;;  -----------------
;;  Starting with Emacs 24.4, listing details are hidden by default.
;;  Use `(' anytime to toggle this hiding.  You can use option
;;  `diredp-hide-details-initially-flag' to change the default/initial
;;  state.  See also option `diredp-hide-details-propagate-flag'.
;;
;;  If you have an Emacs version older than 24.4, you can use library
;;  `dired-details+.el' (plus `dired-details.el') to get similar
;;  behavior.
;;
;; Show details by default in 24.4 (does not work here? configure in dired config)
;; (setq diredp-hide-details-initially-flag nil)
;;
;; Other dired inherit the current setting
;; (setq diredp-hide-details-propagate-flag t)
;;















