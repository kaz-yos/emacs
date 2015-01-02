;;; 500_opening-special-files-related.el ---         -*- lexical-binding: t; -*-

;;;
;;; vlf: View Large Files
;; https://github.com/m00natic/vlfi
(require 'vlf-setup)


;;;
;;; open-junk-file.el for creation of permanent test files
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)
