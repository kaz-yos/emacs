;;; vlf: View Large Files
;; https://github.com/m00natic/vlfi
(require 'vlf-integrate)

;;; open-junk-file.el for creation of permanent test files
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)
