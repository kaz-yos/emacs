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
(use-package org
  :ensure org-plus-contrib
  :defer t
  :mode ("\\.org" . org-mode)
  :bind (;; Suggested bindings for global
         ;; http://orgmode.org/manual/Activation.html#Activation
         ;; ("C-c l" . org-store-link)
         ;; ("C-c a" . org-agenda)
         ;; ("C-c c" . org-capture)
         ;; ("C-c b" . org-iswitchb)
         ;;
         :map org-mode-map
         ;; Key bindings
         ("<C-tab>" . other-window-or-split)
         ("<A-tab>" . org-global-cycle)
         ;; Backslash for LaTeX editing
         (  ";" . my-tex-insert-backslash)
         ("A-;" . my-tex-insert-semicolon)
         ;; sequential-command
         ("C-a" . org-seq-cmd--home)
         ("C-e" . org-seq-cmd--end)
         ;;
         ;; Fix for funny bindings
         ;; The following is mysteriously added to org-mode-map.
         ;; keymap
         ;; (103 . revert-buffer)
         ;; (60 . beginning-of-buffer)
         ;; (62 . end-of-buffer)
         ;; (104 . describe-mode)
         ;; (63 . describe-mode)
         ;; (127 . scroll-down-command)
         ;; (33554464 . scroll-down-command)
         ;; (32 . scroll-up-command)
         ;; (113 . quit-window)
         ;; (57 . digit-argument)
         ;; (56 . digit-argument)
         ;; (55 . digit-argument)
         ;; (54 . digit-argument)
         ;; (53 . digit-argument)
         ;; (52 . digit-argument)
         ;; (51 . digit-argument)
         ;; (50 . digit-argument)
         ;; (49 . digit-argument)
         ;; (48 . digit-argument)
         ;; (45 . negative-argument)
         ;; (remap keymap
         ;;        (self-insert-command . undefined))
         ;; Confirmed these are not from auctex, pdf-tools, cider-overlay
         ;; Fix by hard-coding for now.
         ("g" . org-self-insert-command)
         ("<" . org-self-insert-command)
         (">" . org-self-insert-command)
         ("h" . org-self-insert-command)
         ("?" . org-self-insert-command)
         ("q" . org-self-insert-command)
         ("SPC" . org-self-insert-command)
         ("DEL" . org-delete-backward-char)
         ("0" . org-self-insert-command)
         ("1" . org-self-insert-command)
         ("2" . org-self-insert-command)
         ("3" . org-self-insert-command)
         ("4" . org-self-insert-command)
         ("5" . org-self-insert-command)
         ("6" . org-self-insert-command)
         ("7" . org-self-insert-command)
         ("8" . org-self-insert-command)
         ("9" . org-self-insert-command)
         ("-" . org-self-insert-command)
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
         ("M-s M-s" . org-latex-export-to-pdf-async)
         ("M-s M-l" . org-latex-export-to-latex-save)
         ;;
         ;; org-src mode
         :map org-src-mode-map
         ("M-s M-s" . my-latex-math-preview))
  :init
  ;; https://github.com/jwiegley/use-package#extending-the-load-path
  ;; If using manually installed org-mode.
  (add-to-list 'load-path
               ;; We need a wild card as a MELPA package keeps changing the folder name.
               ;; http://emacs.stackexchange.com/questions/9768/elisp-files-in-load-path-are-not-loaded-on-emacs-start
               ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html
               (car (last (file-expand-wildcards (concat user-emacs-directory
                                                         "elpa/org-plus-contrib*")))))
  ;;
  :config
  ;; sequential-command
  (define-sequential-command org-seq-cmd--home
    org-beginning-of-line beginning-of-buffer seq-return)
  (define-sequential-command org-seq-cmd--end
    org-end-of-line end-of-buffer seq-return)
  ;;
  (defun open-org-pdf-in-app ()
    (interactive)
    (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name))
                            ".pdf"))
          (pdf-app "/Applications/Skim.app"))
      (shell-command (concat "open -a "
                             pdf-app
                             " "
                             pdf-file))))
  ;;
  ;; Swap dimension specification
  (defun my-reverse-org-table-dimension (size)
    (let* ((split (org-split-string size " *x *"))
           (pos1 (nth 1 split))
           (pos0 (car split)))
      (concat pos1 "x" pos0)))
  ;; Wrapper to fix the dimension
  (defun org-table-create-reverse-table-dimension (old-fun &optional size)
    "Query for a size and insert a table skeleton.
SIZE is a string Rows x Columns like for example \"2x3\".
When calling non-interactively SIZE should be a string Columns x Rows
to conform the org-mode convention."
    (interactive "P")
    (unless size
      (setq size (my-reverse-org-table-dimension
                  (read-string
                   (concat "Table size Rows x Columns [e.g. "
                           (my-reverse-org-table-dimension org-table-default-size) "]: ")
                   "" nil (my-reverse-org-table-dimension org-table-default-size)))))
    (funcall old-fun size))
  ;; Advice org-table-create
  (advice-add 'org-table-create
              :around #'org-table-create-reverse-table-dimension)
  ;;
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
  ;; company-mode
  ;; company in org-mode not working #50
  ;; https://github.com/company-mode/company-mode/issues/50
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
  ;; Company-mode completion for Org keywords
  ;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
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
  ;; Non-nil means when yanking subtrees, fold them.
  (setq org-yank-folded-subtrees nil)
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
;;;
;;; Org-Capture
  ;; https://github.com/alphapapa/org-protocol-capture-html
  ;; (when nil
  ;;   (use-package org-capture)
  ;;   (use-package org-protocol)
  ;;   (setq org-capture-templates
  ;;         '(("w" "Web site" entry
  ;;            (file "")
  ;;            "* %a :website:\n\n%U %?\n\n%:initial"))))
  ;;
;;;
;;; org-src.el
  (use-package org-src
    :config
    ;; This does not respect elscreen.
    ;; (advice-add 'org-edit-src-exit
    ;;             :after #'winner-undo)
    ;; (advice-remove 'org-edit-src-exit
    ;;                #'winner-undo)
    ;; Non-nil means turn `auto-save-mode' on when editing a source block.
    (setq org-edit-src-turn-on-auto-save nil)
    ;; When 0 (the default), don't auto-save.
    (setq org-edit-src-auto-save-idle-delay 0)
    ;; This prevents activation of auto-save-timer by `org-src-mode'.
    ;; The timer seems to be the cause of the following error.
    ;; This malformed timer gets into `timer-idle-list'.
    ;; Debugger entered--Lisp error: (wrong-type-argument timerp nil)
    ;; signal(wrong-type-argument (timerp nil))
    ;; cancel-timer(nil)
    ;; #f(compiled-function () #<bytecode 0x5b5493f9>)()
    ;; apply(#f(compiled-function () #<bytecode 0x5b5493f9>) nil)
    ;; timer-event-handler([t 0 5 0 t #f(compiled-function () #<bytecode 0x5b5493f9>) nil idle 0])
    (setq-default org-src--auto-save-timer t))
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
  ;; R interweaving code and results.
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-08/msg00785.html
  (setq org-babel-R-command "R --silent --no-save")
  ;;
  ;; Remove Every Source Block Results
  ;; https://www.wisdomandwonder.com/article/10597/remove-every-source-block-results
  (defconst help/org-special-pre "^\s*#[+]")
  (defun help/org-2every-src-block (fn)
    "Visit every Source-Block and evaluate `FN'."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward (concat help/org-special-pre "BEGIN_SRC") nil t)
          (let ((element (org-element-at-point)))
            (when (eq (org-element-type element) 'src-block)
              (funcall fn element)))))
      (save-buffer)))
  (define-key org-mode-map (kbd "s-]") (lambda () (interactive)
                                         (help/org-2every-src-block
                                          'org-babel-remove-result)))
  ;;
;;;  ob-async.el
  ;; https://github.com/astahlman/ob-async
  (use-package ob-async
    :ensure t
    :disabled t
    :commands (ob-async-org-babel-execute-src-block)
    :init
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))
  ;;
  ;;
;;;
;;; org-ref.el
  ;; https://github.com/jkitchin/org-ref
  ;; https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  ;; https://www.youtube.com/watch?v=2t925KRBbFc&nohtml5=False
  ;; org-ref-pdf.el requires pdf-tools.el
  (use-package org-ref
    :ensure t
    ;; https://github.com/jwiegley/use-package#hooks
    :hook (org-mode . (lambda () (require 'org-ref)))
    :bind (:map org-mode-map
                ("C-A-a" . org-ref-helm-insert-cite-link))
    :config
    ;; To track when loading is occurring.
    (message "org-ref has just been loaded.")
    ;; https://github.com/jkitchin/org-ref/issues/468
    (setq org-ref-show-broken-links nil)
    ;; Set the format for menu.
    (use-package helm-bibtex
      :ensure t
      :config
      ;; Alist of format strings for displaying entries in the results list.
      ;; "${author:N}" is truncated to a width of N characters
      ;; "${title:*}" is truncated to the remaining width in the results
      (setq bibtex-completion-display-formats
            '((t . "${author:36} ${title:72} ${year:4} ${journal:18}"))))
    ;;
    ;; Need to put these configurations within the org file.
    ;; https://github.com/jkitchin/org-ref#screenshots
    ;; https://www.sharelatex.com/learn/Bibtex_bibliography_styles
    ;; http://sites.stat.psu.edu/~surajit/present/bib.htm
    ;; bibliographystyle:unsrt
    ;; bibliography:file_name.bib
    ;;
    ;; (add-hook 'org-mode-hook 'zotelo-minor-mode)
    )
  ;;
;;;  org-ref-citeproc.el
  ;; Citation processor for org-mode
  ;; https://github.com/jkitchin/org-ref/tree/master/citeproc
  ;; http://kitchingroup.cheme.cmu.edu/blog/2015/12/03/Exporting-numbered-citations-in-html-with-unsorted-numbered-bibliography/
  (use-package org-ref-citeproc
    :commands (orcp-citeproc
               org-html-export-to-html-citeproc)
    :config
    (defun org-html-export-to-html-citeproc ()
      "Export as an html file after citeproc."
      (interactive)
      (when (file-exists-p (concat (file-name-base buffer-file-name) ".html"))
        (delete-file (concat (file-name-base buffer-file-name) ".html")))
      (let ((org-export-before-parsing-hook '(orcp-citeproc)))
        (browse-url (org-html-export-to-html))))
    ;;
    ;; Advice the function that inserts the bibliography text to remove junk.
    (defun orcp-formatted-bibliography-clean-zotero-junk (orig-fun)
      ;; Invoke (orcp-formatted-bibliography), which will contain Zotero-related junks.
      ;; https://forums.zotero.org/discussion/61715/prevent-extra-braces-in-bibtex-export
      ;; https://forums.zotero.org/discussion/29127/triple-curly-braces-around-capauthor-in-bibtex-export
      (let ((res (funcall orig-fun)))
        ;; Clean it up
        (replace-regexp-in-string "{\\|}\\|*\\|\\\\'\\|\\\\" "" res)))
    (advice-add 'orcp-formatted-bibliography
                :around #'orcp-formatted-bibliography-clean-zotero-junk)
    ;;
    ;;
    (defun orcp-doi-empty (_orig-fun _entry)
      "Always return an empty doi. I do not use doi."
      ;; Just return an empty string always.
      "")
    (advice-add 'orcp-doi
                :around #'orcp-doi-empty))
  ;;
  ;;
;;;
;;; Exporter configurations
  ;; ox.el --- Export Framework for Org Mode
  (use-package ox
    :config
    (use-package ox-extra
      :config
      ;; Activate :ignore: tag
      ;; https://emacs.stackexchange.com/questions/9492/is-it-possible-to-export-content-of-subtrees-without-their-headings
      (ox-extras-activate '(ignore-headlines))))
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
        (expand-file-name (concat user-emacs-directory
                                  "init_org_async.el")))
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
  ;; $ conda install pygments
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
;;;  ox-md.el
  ;; Regular markdown exporter
  (use-package ox-md)
  ;;
;;;  ox-publish
  ;; Publishing Org-mode files to HTML
  ;; http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
  (use-package ox-publish)
  ;;
;;;  org2blog.el
  ;; blog from org-mode to wordpress
  ;; https://github.com/org2blog/org2blog
  ;; Blogging with Org Mode (Only)
  ;; http://irreal.org/blog/?p=4940
  ;; Mathematics in a Blog Post
  ;; http://irreal.org/blog/?p=3018
  (use-package org2blog
    :ensure t
    ;; Without an universal argument (C-u), these only publish a draft.
    :commands (org2blog/wp-post-buffer
               org2blog/wp-post-buffer-as-page
               my-wp-directly-post-as-draft
               my-wp-directly-post-as-draft-citeproc)
    ;;
    :config
    ;; Do not convert LaTeX to WP latex blocks.
    (setq org2blog/wp-use-wp-latex nil)
    ;; Configure sites
    ;; Association list to set information for each blog.
    ;; Each element of the alist is a blog name.  The CAR of each
    ;; element is a string, uniquely identifying the project.  The CDR
    ;; of each element is a well-formed property list with an even
    ;; number of elements, alternating keys and values, specifying
    ;; parameters for the blog.
    ;; (:property value :property value ... )
    ;; org2blog/wp-blog-alist is configured elsewhere.
    ;;
    ;; M-x org2blog/wp-post-buffer errors as wp.getPageList method requires edit access.
    ;;
    ;; Backtrace
    ;; Debugger entered--Lisp error: (error "XML-RPC fault ‘Sorry, you are not allowed to edit pages.’")
    ;; signal(error ("XML-RPC fault ‘Sorry, you are not allowed to edit pages.’"))
    ;; error("XML-RPC fault `%s'" "Sorry, you are not allowed to edit pages.")
    ;; xml-rpc-xml-to-response(((methodResponse nil (fault nil (value nil (struct nil (member nil (name nil "faultCode") (value nil (int nil "401"))) (member nil (name nil "faultString") (value nil (string nil "Sorry, you are not allowed to edit pages.")))))))))
    ;; xml-rpc-method-call("https://some_url.com/xmlrpc.php" "wp.getPageList" "1" "username" "password_string\n")
    ;; wp-get-pagelist("https://some_url.com/xmlrpc.php" "username" "password_string\n" "1")
    ;; org2blog/wp-login(nil)
    ;; org2blog/wp-correctly-login()
    ;; org2blog/wp-post-buffer(nil)
    ;; funcall-interactively(org2blog/wp-post-buffer nil)
    ;; call-interactively(org2blog/wp-post-buffer record nil)
    ;; command-execute(org2blog/wp-post-buffer record)
    ;; helm-M-x(nil "org2blog/wp-post-buffer")
    ;; funcall-interactively(helm-M-x nil "org2blog/wp-post-buffer")
    ;; call-interactively(helm-M-x nil nil)
    ;; command-execute(helm-M-x)
    ;;
    ;; Define a function that avoids (org2blog/wp-correctly-login) entirely.
    (defun my-wp-directly-post-as-draft ()
      "Directly post as a draft."
      (interactive)
      (let* ((org2blog/wp-blog-name (or
                                     ;; OR Use the only entry in alist
                                     (and (equal (length org2blog/wp-blog-alist) 1)
                                          (car (car org2blog/wp-blog-alist)))
                                     ;; OR Prompt user
                                     (completing-read
                                      "Blog to login into? ([Tab] to see list): "
                                      (mapcar 'car org2blog/wp-blog-alist) nil t)))
             (dummy-variable (unless (> (length org2blog/wp-blog-name) 1)
                               (error "Invalid blog name")))
             (org2blog/wp-blog (assoc org2blog/wp-blog-name org2blog/wp-blog-alist))
             (org2blog/wp-server-xmlrpc-url (plist-get (cdr org2blog/wp-blog) :url))
             (org2blog/wp-server-userid (plist-get (cdr org2blog/wp-blog) :username))
             (org2blog/wp-server-blogid (or (plist-get (cdr org2blog/wp-blog) :id) "1"))
             (org2blog/wp-server-pass (or
                                       (plist-get (cdr org2blog/wp-blog) :password)
                                       (read-passwd (format "%s Weblog password? " org2blog/wp-blog-name)))))
        ;; Body
        ;; Directly post as a draft to avoid a failed login.
        (metaweblog-new-post org2blog/wp-server-xmlrpc-url
                             org2blog/wp-server-userid
                             org2blog/wp-server-pass
                             org2blog/wp-server-blogid
                             ;; Convert current buffer as post.
                             (org2blog/wp--export-as-post nil)
                             ;; Do not publish (draft only).
                             nil)))
    ;;
    (defun my-wp-directly-post-as-draft-citeproc ()
      "Directly post as a draft after processing citations."
      (interactive)
      ;; orcp-citeproc is done before parsing html.
      (let ((org-export-before-parsing-hook '(orcp-citeproc)))
        (my-wp-directly-post-as-draft)))
    ;;
    ;; Connect code and result for datascienceplus.com
    (defun org2blog/wp--export-as-html-ds+ (html)
      "Clean up for DS+."
      (replace-regexp-in-string "</pre>
</div>

<pre class=\"example\">"
                                ""
                                html
                                nil 'literal))
    (advice-add 'org2blog/wp--export-as-html
                :filter-return #'org2blog/wp--export-as-html-ds+)
    )
  ;;
  ;; ox-html.el
  ;; ox-wp delegates most work to ox-html.el
  (use-package ox-html
    :commands (org-html-paragraph
               org-html-src-block)
    :config
    ;; Output type to be used by htmlize when formatting code snippets.
    ;; https://emacs.stackexchange.com/questions/7629/the-syntax-highlight-and-indentation-of-source-code-block-in-exported-html-file
    ;; Do not format.
    (setq org-html-htmlize-output-type nil)
    ;;
    ;; Redefine html paragraph parser to avoid unnecessary <p> </p>.
    (defun org-html-paragraph (paragraph contents info)
      "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
      (let* ((parent (org-export-get-parent paragraph))
	     (parent-type (org-element-type parent))
	     (style '((footnote-definition " class=\"footpara\"")
		      (org-data " class=\"footpara\"")))
	     (attributes (org-html--make-attribute-string
		          (org-export-read-attribute :attr_html paragraph)))
	     (extra (or (cadr (assq parent-type style)) "")))
        (cond
         ((and (eq parent-type 'item)
	       (not (org-export-get-previous-element paragraph info))
	       (let ((followers (org-export-get-next-element paragraph info 2)))
	         (and (not (cdr followers))
		      (memq (org-element-type (car followers)) '(nil plain-list)))))
          ;; First paragraph in an item has no tag if it is alone or
          ;; followed, at most, by a sub-list.
          contents)
         ((org-html-standalone-image-p paragraph info)
          ;; Standalone image.
          (let ((caption
	         (let ((raw (org-export-data
			     (org-export-get-caption paragraph) info))
		       (org-html-standalone-image-predicate
		        #'org-html--has-caption-p))
	           (if (not (org-string-nw-p raw)) raw
		     (concat "<span class=\"figure-number\">"
			     (format (org-html--translate "Figure %d:" info)
				     (org-export-get-ordinal
				      (org-element-map paragraph 'link
                                        #'identity info t)
				      info nil #'org-html-standalone-image-p))
			     " </span>"
			     raw))))
	        (label (and (org-element-property :name paragraph)
			    (org-export-get-reference paragraph info))))
	    (org-html--wrap-image contents info caption label)))
         ;; Regular paragraph.
         ;; Do not use <p> </p>
         (t (format "%s" contents)))))
    ;;
    ;; Emphasize example.
    (defun org-html-example-block (example-block _contents info)
      "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      (let ((attributes (org-export-read-attribute :attr_html example-block)))
        (if (plist-get attributes :textarea)
	    (org-html--textarea-block example-block)
          (format "<pre class=\"example\"%s><em>\n%s</em></pre>"
	          (let* ((name (org-element-property :name example-block))
		         (a (org-html--make-attribute-string
			     (if (or (not name) (plist-member attributes :id))
			         attributes
			       (plist-put attributes :id name)))))
		    (if (org-string-nw-p a) (concat " " a) ""))
	          (org-html-format-code example-block info)))))
    ;;
    ;; This is not working.
    (defun org-html-src-block (src-block _contents info)
      "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
      (if (org-export-read-attribute :attr_html src-block :textarea)
          (org-html--textarea-block src-block)
        (let* ((lang (org-element-property :language src-block))
	       (code (org-html-format-code src-block info))
	       (label (let ((lbl (and (org-element-property :name src-block)
				      (org-export-get-reference src-block info))))
		        (if lbl (format " id=\"%s\"" lbl) "")))
	       (klipsify  (and  (plist-get info :html-klipsify-src)
                                (member lang '("javascript" "js"
					       "ruby" "scheme" "clojure" "php" "html")))))
          ;; Use <em> </em> for example
          (if (not lang) (format "<pre class=\"example\"%s><em>\n%s</em></pre>" label code)
	    (format "<div class=\"org-src-container\">\n%s%s\n</div>"
		    ;; Build caption.
		    (let ((caption (org-export-get-caption src-block)))
		      (if (not caption) ""
		        (let ((listing-number
			       (format
			        "<span class=\"listing-number\">%s </span>"
			        (format
			         (org-html--translate "Listing %d:" info)
			         (org-export-get-ordinal
			          src-block info nil #'org-html--has-caption-p)))))
		          (format "<label class=\"org-src-name\">%s%s</label>"
			          listing-number
			          (org-trim (org-export-data caption info))))))
		    ;; Contents.
		    (if klipsify
		        (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
			        lang
			        label
			        (if (string= lang "html")
				    " data-editor-type=\"html\""
			          "")
			        code)
		      (format "<pre class=\"src src-%s\"%s>%s</pre>"
                              lang label code))))))))
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
    :ensure t
    :commands (org-bullets-mode)
    ;;
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))
