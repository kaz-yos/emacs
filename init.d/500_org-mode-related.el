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
  :bind (;; Suggested bindings for global
         ;; http://orgmode.org/manual/Activation.html#Activation
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ;;
         :map org-mode-map
         ;; Key bindings
         ("<C-tab>" . other-window-or-split)
         ("<A-tab>" . org-global-cycle)
         ;; Backslash
         (  ";" . my-tex-insert-backslash)
         ("A-;" . my-tex-insert-semicolon)
         ;;
         ;; bm.el-like function
         ;; ("s-b" . helm-org-in-buffer-headings)
         ;;
         ;; This is taken by icicle. Reclaim.
         ("C-c '" . org-edit-special)
         ;;
         ;; Arrow key replacement for HHKB
         ("A-M-i" . org-metaup)
         ("A-M-k" . org-metadown)
         ("A-M-l" . org-metaright)
         ("A-M-j" . org-metaleft)
         ;; These are not working?
         ("A-S-M-i" . org-shiftmetaup)
         ("A-S-M-k" . org-shiftmetadown)
         ("A-S-M-l" . org-shiftmetaright)
         ("A-S-M-j" . org-shiftmetaleft)
         ;;
         ;; This breaks other behaviors?
         ;; ("<return>" . org-meta-return)
         ;; Swap these keys
         ;; ("<return>" . org-return-indent)
         ;; ("C-j" . org-return)
         ;; Keys
         ("A-s" . org-latex-export-to-pdf-async)
         ("H-s" . org-latex-export-to-pdf-async)
         ("A-l" . org-latex-export-to-latex-save)
         ("H-l" . org-latex-export-to-latex-save))
  :init
  ;; https://github.com/jwiegley/use-package#extending-the-load-path
  ;; If using manually installed org-mode.
  (add-to-list 'load-path
               ;; We need a wild card as a MELPA package keeps changing the folder name.
               ;; http://emacs.stackexchange.com/questions/9768/elisp-files-in-load-path-are-not-loaded-on-emacs-start
               ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html
               (car (last (file-expand-wildcards "~/.emacs.d/elpa/org-plus-contrib*"))))
  ;;
  :config
  (defun read-only-mode-off (&optional arg)
    (read-only-mode -1))
  (advice-add 'org-edit-special
              :before 'read-only-mode-off)
  ;;
  ;; Treat $ as a punctuation "." to avoid yasnippet malfunction.
  ;; Treating $ as a white space " " causes trailing $ removal.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html
  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
  (modify-syntax-entry ?$  "."  text-mode-syntax-table)
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
  ;; Agenda related
  ;; http://mbork.pl/2017-09-18_How_to_hide_repeating_entries_from_the_Org_agenda
  (setq org-agenda-show-future-repeats nil)
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
;;;  Declare languages to load.
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
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lisp.html
     (lisp . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
     (python . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
     (ditaa . t)
     ;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
     (dot . t)
     ;;
     (emacs-lisp . t)
     (shell . t)))
  ;; Do not ask code execution confirmation
  (setq org-confirm-babel-evaluate nil)
  ;; Org-mode: Source block doesn't respect parent buffer indentation
  ;; http://emacs.stackexchange.com/questions/9472/org-mode-source-block-doesnt-respect-parent-buffer-indentation
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)
  ;; Source code fontification
  ;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
  (setq org-src-fontify-natively t)
  ;; Source code tab key behavior
  ;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-org.org#code-block-fontification
  (setq org-src-tab-acts-natively t)
  ;;
  ;; Delay before saving a source code buffer back into its base buffer.
  (setq org-edit-src-auto-save-idle-delay 5)
  ;;
  ;; Language specific settings
  ;; ditaa diagram language (git repo org-mode has necessary scripts)
  (setq org-ditaa-jar-path "~/.emacs.d/packages/org-mode/contrib/scripts/ditaa.jar")
  ;; R interweaving code and results.
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-08/msg00785.html
  (setq org-babel-R-command "R --silent --no-save")
  ;;
;;;  ob-async.el
  ;; https://github.com/astahlman/ob-async
  (use-package ob-async
    :disabled t
    :commands (ob-async-org-babel-execute-src-block)
    :init
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))
  ;;
  ;;
;;;
;;; org-mode citation management by org-ref
  ;; https://github.com/jkitchin/org-ref
  ;; https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  ;; https://www.youtube.com/watch?v=2t925KRBbFc&nohtml5=False
  ;; org-ref-pdf.el requires pdf-tools.el
  (use-package org-ref
    :init
    (define-key org-mode-map (kbd "C-A-a") 'org-ref-helm-insert-cite-link)
    ;;
    ;; Need to put these configurations within the org file.
    ;; https://github.com/jkitchin/org-ref#screenshots
    ;; https://www.sharelatex.com/learn/Bibtex_bibliography_styles
    ;; http://sites.stat.psu.edu/~surajit/present/bib.htm
    ;; bibliographystyle:unsrt
    ;; bibliography:file_name.bib
    ;;
    (add-hook 'org-mode-hook 'zotelo-minor-mode))
  ;;
  ;;
;;;
;;; Exporter configurations
  ;; ox.el --- Export Framework for Org Mode
  (use-package ox)
  ;;
  ;; Asynchronous export is explained in ox.el
  ;; `org-export-in-background'
  ;; Non-nil means export and publishing commands will run in background.
  ;; https://www.gnu.org/software/emacs/manual/html_node/org/The-Export-Dispatcher.html
  ;; (setq org-export-in-background t)
  ;;
  ;; `org-export-async-init-file'
  ;; File used to initialize external export process.
  ;;  Absolute path: That init.el is used for the external emacs process.
  ;;  nil: The regular init.el is used (slow).
  ;; `org-export-async-start' calls another emacs with -Q -l org-export-async-init-file
  ;;
  ;; References
  ;; org-mode ML: `org-export-async-init-file'
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2013-09/msg01299.html
  ;; ode 8 async export process fails
  ;; http://superuser.com/questions/738492/org-mode-8-async-export-process-fails
  ;; org-mode ML: async export not working for me
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2014-09/msg00464.html
  ;; org-mode ML: Very minimal async export example does not work
  ;; http://osdir.com/ml/emacs-orgmode-gnu/2013-10/msg00939.html
  ;; A minimum example
  ;; https://github.com/russell/dotfiles/blob/master/emacs.d/init-org-export.el
  ;; Spacemacs example
  ;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bemacs/org/local/org-async-init.el
  ;; Example of One unified file for both usual and async org-mode
  ;; https://github.com/dzop/emacs.d/blob/master/core/init/org/async-init.el
  ;; https://github.com/dzop/emacs.d/blob/master/core/init/init-org-mode.el
  (setq org-export-async-init-file
        ;; Need to be a full path.
        (expand-file-name "~/.emacs.d/init_org_async.el"))
  ;; Check errors in async process with M-x org-export-stack
  ;;
  ;; `org-export-async-debug'
  ;; Non-nil means asynchronous export process should leave data behind.
  ;; This data is found in the appropriate "*Org Export Process*"
  ;; buffer, and in files prefixed with "org-export-process" and
  ;; located in ‘temporary-file-directory’.
  (setq org-export-async-debug t)
  ;;
  ;; https://stackoverflow.com/questions/3034237/check-if-current-emacs-buffer-contains-a-string
  (defun buffer-contains-substring (string)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t))))
  ;; Async export with direct key bindings
  (defun org-latex-export-to-pdf-async (&optional SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)
    "Export to pdf async directly

This is a custom version of org-latex-export-to-pdf with an async flag."
    (interactive)
    (save-buffer)
    (if (buffer-contains-substring "#+LATEX_CLASS: beamer")
        ;; If the header contains ": beamer", export as a beamer presentation.
        (org-beamer-export-to-pdf t SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)
      (org-latex-export-to-pdf t SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)))
  ;;
  (defun org-latex-export-to-latex-save ()
    "Export to latex .tex file after saving."
    (interactive)
    (save-buffer)
    (org-latex-export-to-latex))
  ;;
  ;; auto-revert in org-stack-mode
  (add-hook 'org-export-stack-mode-hook #'turn-on-auto-revert-mode)
  ;;
;;;  LaTeX in org-mode
  ;; The Org-article LaTeX class
  ;; http://orgmode.org/worg/org-contrib/babel/examples/article-class.html
  ;; 12.7.5 LaTeX specific attributes
  ;; http://orgmode.org/manual/LaTeX-specific-attributes.html
  ;;
  ;; PDF processing with correct bibtex handling
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-05/msg00791.html
  ;; %f: full file name
  ;; %b: file base name
  ;; %o: base directory
  ;; latexmk is a high-level tool to integrate low-level tools like platex and bibtex.
  (setq org-latex-pdf-process
        '("latexmk -f %f"))
  ;;
  ;; Remove additional temporary files.
  (setq org-latex-logfiles-extensions
        (append '("dvi" "bbl") org-latex-logfiles-extensions))
  ;;
  (setq org-highlight-latex-fragments-and-specials t)
  ;; http://stackoverflow.com/questions/17239273/org-mode-buffer-latex-syntax-highlighting
  ;; ‘latex’    Highlight LaTeX snippets and environments. (not full LaTeX syntax highlighting)
  ;; ‘script’   Highlight subscript and superscript.
  ;; ‘entities’ Highlight entities.
  (setq org-highlight-latex-and-related '(latex
                                          script
                                          entities))
  ;;
  ;; org-latex-classes
  ;; Alist of LaTeX classes and associated header and structure.
  ;; If #+LATEX_CLASS is set in the buffer, use its value and the
  ;; associated information.  Here is the structure of each cell:
  ;;
  ;; Japanese article configuration
  ;; http://d.hatena.ne.jp/mokimokisan/20120624/1340558857
  ;; http://fjyuu.info/blog/org-export-pdf/
  ;; https://texwiki.texjp.org/?Emacs%2FOrg%20mode#a97921ad
  ;; http://qiita.com/kawabata@github/items/1b56ec8284942ff2646b
  ;; https://ryogan.org/blog/2015/12/31/emacs-org-mode-から-latex-export-する/
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[a4j]{jsarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;;
  ;; Additional font locks
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             ;; (font-lock-add-keywords MODE KEYWORDS &optional HOW)
  ;;             (font-lock-add-keywords nil
  ;;                                     '(("\\\\" 1
  ;;                                        font-latex-warning-face t)))))
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
  ;; A string will be inserted as-is in the header of the document. Use two backslashes.
  ;; Set to nil avoid a opaque setting. Non-default packages should be loaded explicitly.
  (setq org-latex-packages-alist '())
  ;;
  ;;
;;;   PDF export with syntax highlighting
  ;; http://joat-programmer.blogspot.cl/2013/07/org-mode-version-8-and-pdf-export-with.html
  ;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
  ;; https://github.com/syl20bnr/spacemacs/issues/7055
  ;; https://www.sharelatex.com/learn/Code_Highlighting_with_minted
  ;; https://github.com/gpoore/minted/issues/9
  ;; https://emacs.stackexchange.com/questions/24283/org-mode-converting-spaces-to-tabs-when-evaluating-source
  ;;
  ;; How to export source code. See the help for `org-latex-listings'.
  ;; t for listings or 'minted for minted (depends on pygments).
  ;; Also install pygments and give the -shell-escape option to the LaTeX engine.
  (setq org-latex-listings 'minted)
  ;;
  ;; Association list of options for the latex minted package. Set in the file.
  ;; These options are supplied within square brackets in
  ;; \begin{minted} environments.  Each element of the alist should
  ;; be a list containing two strings: the name of the option, and the value.
  ;; http://ctan.sharelatex.com/tex-archive/macros/latex/contrib/minted/minted.pdf
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("linenos" "false")))
  ;;
  ;; `org-latex-minted-langs'
  ;; Alist mapping languages to their minted language counterpart.
  ;;
  ;;
;;;   org-edit-latex.el
  ;; https://github.com/et2010/org-edit-latex
  (use-package org-edit-latex
    :diminish org-edit-latex-mode
    :config
    (add-hook 'org-mode-hook 'org-edit-latex-mode)
    ;; This is default.
    (setq org-edit-latex-frag-master "frag-master.tex")
    ;; whether to create a TeX-master file specified in org-edit-latex-frag-master.
    ;; default file name "frag-master.tex".
    (setq org-edit-latex-create-master nil))
  ;;
;;;  Beamer presentations using the new export engine
  ;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html
  (use-package ox-beamer)
  ;; bold is bold not \alert{} (can appear as red text)
  ;; http://orgmode.org/worg/exporters/beamer/ox-beamer.html#export-filters
  (defun my-beamer-bold (contents backend info)
    (when (eq backend 'beamer)
      (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))
  (add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)
  ;;
;;;  Regular markdown exporter
  (use-package ox-md)
  ;;
;;;  Qiita Markdown Back-End for Org Export Engine
  ;; https://github.com/0x60df/ox-qmd
  ;; http://qiita.com/0x60df/items/3cde67967e3db30d9afe
  (use-package ox-qmd)
  ;;
;;;  Publishing Org-mode files to HTML
  ;; http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
  (use-package ox-publish)
  ;;
;;;  org2jekyll.el
  ;; https://github.com/ardumont/org2jekyll#installsetup
  ;; jekyll installation
  ;; https://jekyllrb.com/docs/troubleshooting/#jekyll-amp-mac-os-x-1011
  (use-package org2jekyll)
  ;; org2jekyll is about:
  ;;  - writing comfortably leveraging org-mode
  ;;  - converting your org-mode blog or page to html leveraging org-publish
  ;;  - and see the rendering result in the browser with jekyll
  ;; Set up example
  ;; https://github.com/ardumont/org2jekyll#setup
  ;; https://github.com/ardumont/blog-pack/blob/master/blog-pack.el
  (setq org2jekyll-blog-author "kaz-yos")
  (setq org2jekyll-source-directory  (expand-file-name "~/Documents/_web/org"))
  (setq org2jekyll-jekyll-directory  (expand-file-name "~/Documents/_web/html"))
  (setq org2jekyll-jekyll-drafts-dir "")
  (setq org2jekyll-jekyll-posts-dir  "_posts/")
  ;; Association list to control publishing behavior. (defined in ox-publish.el)
  (setq org-publish-project-alist
        `(("default"
           :base-directory ,(org2jekyll-input-directory)
           :base-extension "org"
           ;; :publishing-directory "/ssh:user@host:~/html/"
           :publishing-directory ,(org2jekyll-output-directory)
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :section-numbers nil
           :with-toc nil
           :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
           :html-preamble t
           :recursive t
           :make-index t
           :html-extension "html"
           :body-only t)
          ("post"
           :base-directory ,(org2jekyll-input-directory)
           :base-extension "org"
           :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :section-numbers nil
           :with-toc nil
           :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
           :html-preamble t
           :recursive t
           :make-index t
           :html-extension "html"
           :body-only t)
          ("images"
           :base-directory ,(org2jekyll-input-directory "img")
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory ,(org2jekyll-output-directory "img")
           :publishing-function org-publish-attachment
           :recursive t)
          ("js"
           :base-directory ,(org2jekyll-input-directory "js")
           :base-extension "js"
           :publishing-directory ,(org2jekyll-output-directory "js")
           :publishing-function org-publish-attachment
           :recursive t)
          ("css"
           :base-directory ,(org2jekyll-input-directory "css")
           :base-extension "css\\|el"
           :publishing-directory ,(org2jekyll-output-directory "css")
           :publishing-function org-publish-attachment
           :recursive t)
          ("web" :components ("images" "js" "css"))))
  ;;
  ;;
;;;
;;; Miscellaneous configurations
  ;; External applications for opening ‘file:path’ items in a document.
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ;; Drop default for pdf, which means Preview.app in macOS
          ;; ("\\.pdf\\'" . default)
          ))
  ;;
  ;; macOS-specific file-app associations
  ;; http://emacs.stackexchange.com/questions/2856/how-to-configure-org-mode-to-respect-system-specific-default-applications-for-ex
  (setq org-file-apps-defaults-macosx
        '((remote . emacs)
          (system . "open %s")
          ("ps.gz" . "gv %s")
          ("eps.gz" . "gv %s")
          ("dvi" . "xdvi %s")
          ("fig" . "xfig %s")
          ("pdf" . "open -a Skim.app %s")
          (t . "open %s")))
  ;;
  ;; Pretty bullets
  ;; https://github.com/sabof/org-bullets
  (use-package org-bullets
    :commands (org-bullets-mode)
    ;;
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))
