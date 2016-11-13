;;; Org-mode related
;;
;; Installation:
;; http://orgmode.org/manual/Installation.html
;; Use org elpa http://orgmode.org/elpa.html
;; Install org-plus-contrib from org repository. Does not contain scripts.
;;
;; git repository. Contains scripts.
;; $ git clone git://orgmode.org/org-mode.git
;; $ make autoloads
;;
;; To inactivate org that came with emacs. Delete the folder (e.g., in Emacs.app).
;;
;; Load org
(use-package org
  :mode ("\\.org" . org-mode)
  ;; If using downloaded version
  :init
  ;; If using manually installed org-mode.
  ;; (add-to-list 'load-path "~/.emacs.d/packages/org-mode/lisp")
  ;; (add-to-list 'load-path "~/.emacs.d/packages/org-mode/contrib/lisp")
  ;;
  :config
  ;; Suggested bindings for global
  ;; http://orgmode.org/manual/Activation.html#Activation
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)
  ;; Key bindings
  (define-key org-mode-map (kbd "<C-tab>") 'other-window-or-split)
  (define-key org-mode-map (kbd "<A-tab>") 'org-global-cycle)
  (define-key org-mode-map (kbd "A-s") 'org-latex-export-to-pdf)
  (define-key org-mode-map (kbd "A-C-s") 'org-beamer-export-to-pdf)
  ;; Backslash
  (define-key org-mode-map (kbd   ";") 'my-tex-insert-backslash)
  (define-key org-mode-map (kbd "A-;") 'my-tex-insert-semicolon)
  ;;
  ;; bm.el-like function
  (define-key org-mode-map (kbd "s-b") 'helm-org-in-buffer-headings)
  ;;
  ;; Arrow key replacement for HHKB
  (define-key org-mode-map (kbd "A-M-i") 'org-metaup)
  (define-key org-mode-map (kbd "A-M-k") 'org-metadown)
  (define-key org-mode-map (kbd "A-M-l") 'org-metaright)
  (define-key org-mode-map (kbd "A-M-j") 'org-metaleft)
  (define-key org-mode-map (kbd "A-S-M-i") 'org-shiftmetaup)
  (define-key org-mode-map (kbd "A-S-M-k") 'org-shiftmetadown)
  (define-key org-mode-map (kbd "A-S-M-l") 'org-shiftmetaright)
  (define-key org-mode-map (kbd "A-S-M-j") 'org-shiftmetaleft)
  ;;
  ;; This breaks other behaviors?
  ;; (define-key org-mode-map (kbd "<return>") 'org-meta-return)
  ;;
  ;; Auto-complete mode
  (require 'org-ac)
  ;;
  ;; Company mode
  ;; https://github.com/company-mode/company-mode/issues/50
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
  ;;
  ;; Linewrap in Org-mode of Emacs
  ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs
  (setq org-startup-truncated nil)
  (define-key org-mode-map (kbd "M-q") 'toggle-truncate-lines)
  ;;
  ;; Do not fold at startup
  (setq org-startup-folded nil)
  ;; Indent at startup. This causes some strange behavior.
  ;; (setq org-startup-indented t)
  ;;
  ;; TODO related
  ;; Information to record when a task moves to the DONE state.
  ;; nil     Don’t add anything, just change the keyword
  ;; time    Add a time stamp to the task
  ;; note    Prompt for a note and add it with template ‘org-log-note-headings’
  (setq org-log-done 'time)
  ;;
  ;;
;;;
;;; Org-Babel
  ;; org-mode manual 14 Working with source code
  ;; http://orgmode.org/manual/Working-With-Source-Code.html#Working-With-Source-Code
  ;; Active code in Org-mode
  ;; http://orgmode.org/worg/org-contrib/babel/index.html
  ;; Introduction
  ;; http://orgmode.org/worg/org-contrib/babel/intro.html
  ;; Babel Languages
  ;; http://orgmode.org/worg/org-contrib/babel/languages.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
     (latex . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-R.html
     (R . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-stan.html
     (stan . t)
     ;; https://github.com/gjkerns/ob-julia/blob/master/ob-julia-doc.org
     (julia . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
     (clojure . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
     (python . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
     (ditaa . t)
     (emacs-lisp . t)
     (shell . t)))
  ;;
  ;; Source code fontification
  ;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
  (setq org-src-fontify-natively t)
  ;; Source code tab key behavior
  ;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-org.org#code-block-fontification
  (setq org-src-tab-acts-natively t)
  ;;
  ;; Language specific settings
  ;; ditaa diagram language (git repo org-mode has necessary scripts)
  (setq org-ditaa-jar-path "~/.emacs.d/packages/org-mode/contrib/scripts/ditaa.jar")
  ;;
  ;;
;;;
;;; LaTeX in org-mode
  (setq org-highlight-latex-fragments-and-specials t)
  ;; http://stackoverflow.com/questions/17239273/org-mode-buffer-latex-syntax-highlighting
  ;; ‘latex’    Highlight LaTeX snippets and environments. (not full LaTeX syntax highlighting)
  ;; ‘script’   Highlight subscript and superscript.
  ;; ‘entities’ Highlight entities.
  (setq org-highlight-latex-and-related '(latex
                                          script
                                          entities))
  ;;
  ;; ‘org-latex-default-packages-alist’ contains required packages
  ;; The packages in this list are needed by one part or another of
  ;; Org mode to function properly:
  ;; - inputenc, fontenc:  for basic font and character selection
  ;; - graphicx: for including images
  ;; - grffile: allow periods and spaces in graphics file names
  ;; - longtable: For multipage tables
  ;; - wrapfig: for figure placement
  ;; - rotating: for sideways figures and tables
  ;; - ulem: for underline and strike-through
  ;; - amsmath: for subscript and superscript and math environments
  ;; - textcomp, amssymb: for various symbols used
  ;; for interpreting the entities in ‘org-entities’.  You can skip
  ;; some of these packages if you don’t use any of their symbols.
  ;; - capt-of: for captions outside of floats
  ;; - hyperref: for cross references
  ;;
  ;; A cell is of the format ("options" "package" SNIPPET-FLAG COMPILERS)
  ;; The default is configured as follows
  ;; (setq org-latex-default-packages-alist '(("AUTO" "inputenc" t ("pdflatex"))
  ;;                                          ("T1" "fontenc" t ("pdflatex"))
  ;;                                          ("" "graphicx" t)
  ;;                                          ("" "grffile" t)
  ;;                                          ("" "longtable" nil)
  ;;                                          ("" "wrapfig" nil)
  ;;                                          ("" "rotating" nil)
  ;;                                          ("normalem" "ulem" t)
  ;;                                          ("" "amsmath" t)
  ;;                                          ("" "textcomp" t)
  ;;                                          ("" "amssymb" t)
  ;;                                          ("" "capt-of" nil)
  ;;                                          ("" "hyperref" nil)))
  ;;
  ;; Alist of packages to be inserted in every LaTeX header.
  ;; These will be inserted after ‘org-latex-default-packages-alist’.
  ;; Each element is either a cell or a string.
  ;; A cell is of the format: ("options" "package" SNIPPET-FLAG)
  ;; A string will be inserted as-is in the header of the document.
  (setq org-latex-packages-alist '(("" "tikz" t )
                                   ;; String insertion for setting options
                                   "\\tolerance=1000"))
  ;;
  ;;
;;;
;;; org-mode citation management
  ;; https://github.com/jkitchin/org-ref
  ;; https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  ;; https://www.youtube.com/watch?v=2t925KRBbFc&nohtml5=False
  (require 'org-ref)
  (define-key org-mode-map (kbd "C-A-a") 'org-ref-helm-insert-cite-link)
  ;;
  ;; Need to put these configurations within the org file.
  ;; https://github.com/jkitchin/org-ref#screenshots
  ;; https://www.sharelatex.com/learn/Bibtex_bibliography_styles
  ;; http://sites.stat.psu.edu/~surajit/present/bib.htm
  ;; bibliographystyle:unsrt
  ;; bibliography:file_name.bib
  ;;
  ;; PDF processing with correct bibtex handling
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-05/msg00791.html
  ;; %f: full file name
  ;; %b: file base name
  ;; %o: base directory
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;;
  ;;
;;;
;;; Optional exporters
  ;; Beamer presentations using the new export engine
  ;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html
  (require 'ox-beamer)
  ;;
  ;;
;;;
;;; Miscellaneous configurations
  ;; Pretty bullets
  ;; https://github.com/sabof/org-bullets
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
