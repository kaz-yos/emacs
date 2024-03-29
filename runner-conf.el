;;; runner-names
;; List holding the names of file types as strings
("ext-R"
 "ext-pdf"
 "ext-sh"
 "ext-sas"
 "ext-lsf")

;;; runner-alist
;;
;;  An alist holding types associated with a set of commands. Each
;;   type has the following structure:
;;  NAME         ;; Unique name used as key
;;  PATTERN-type ;; Sets the pattern type (filename or extension)
;;               ;; "A list of space-separated extension regexps. Ex. jpe?g gif png (case-insensitive)" 0
;;               ;; "Regexp on file name." 1
;;  PATTERN      ;; A regexp
;;
;;  COMMAND-LIST ;; A list of lists holding commands with the following structure:
;;     LABEL     ;; Optional command label
;;     COMMAND   ;; Command string
;;     PRIORITY  ;; Priority of processing this command. Default is 5.
(("ext-R" 0 "R"
  (("Original Rscript" "{run:out} Rscript" 5)
   ("Project-specific Rscript with tee" "{run:out} ./Rscriptee" 3)
   ("Global Rscript with tee" "{run:out} Rscriptee" 0)))
 ;;
 ("ext-pdf" 0 "pdf"
  (("open command" "open" 0)
   ("Skim" "open -a /Applications/Skim.app" 5)))
 ;;
 ("ext-sh" 0 "sh"
  (("sh" "{run:out} sh" 0)
   ("bash" "{run:out} bash" 5)))
 ;;
 ("ext-sas" 0 "sas"
  (("channing-sas" "qbs" 0)
   ("channing-sas 1G mem 60GB disk" "qbs -q l -o lx,memsize=10000M,disksize=60G" 0)
   ("channing-sas 50GB disk" "qbs -o disksize=50G" 0)
   ("sas" "{run:out} sas" 5)))
 ;;
 ("ext-lsf" 0 "lsf"
  (("bsub" "{run:out} bsub < " 0))))

;;; Run these after reconfiguring.
;; (progn (runner-settings-load) (runner-reset))
