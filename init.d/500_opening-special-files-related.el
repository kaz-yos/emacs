;;; 500_opening-special-files-related.el ---         -*- lexical-binding: t; -*-

;;;
;;; vlf: View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf
  :commands (vlf)
  :config
  (vlf-setup))


;;;
;;; open-junk-file.el for creation of permanent test files
(use-package open-junk-file
  :bind (("C-x C-z" . open-junk-file))
  :config
  (setq open-junk-file-directory "~/junk/%Y/%m/%d-%H%M%S."))
