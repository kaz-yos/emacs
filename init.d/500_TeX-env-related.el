;;; LaTeX environments
;;
;;;
;;; AUCtex
;; http://www.gnu.org/software/auctex/index.html
;; http://www.gnu.org/software/auctex/manual/auctex.html
;;
;; Bare minimum
;; http://www.gnu.org/software/auctex/manual/auctex.html#Quick-Start
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;
;; http://emacsworld.blogspot.com/2011/05/auctex-tip-automatically-save-file.html
(setq TeX-save-query nil) ; autosave before compiling
;;
;;; TeX-fold-mode on by default (C-c C-b C-o to actually fold)
;; http://tex.stackexchange.com/questions/52179/what-is-your-favorite-emacs-and-or-auctex-command-trick
;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    (TeX-fold-mode 1)))
;;
;;; Avoid font size changes 2013-10-14
;; http://stackoverflow.com/questions/9534239/emacs-auctex-latex-syntax-prevents-monospaced-font
;; Only change sectioning colour
(setq font-latex-fontify-sectioning 'color)
;; (setq font-latex-fontify-sectioning (quote nil))
;; super-/sub-script on baseline
(setq font-latex-script-display (quote (nil)))
;; Do not change super-/sub-script font
;;
;;; Exclude bold/italic from keywords
(setq font-latex-deactivated-keyword-classes
      '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))
;;
;;
;;; Special key settings
;;
;; TeX-insert-backslash
(defun my-tex-insert-backslash ()
  (interactive)
  (insert "\\"))
(defun my-tex-insert-forwardslash ()
  (interactive)
  (insert "/"))
(defun my-tex-insert-semicolon ()
  (interactive)
  (insert ";"))
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (local-set-key (kbd   ";") 'my-tex-insert-backslash)
             (local-set-key (kbd "A-;") 'my-tex-insert-semicolon)))
;;
;;;
;;; Japanese setting etc
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX
;; Hiragino font settings. Done in shell
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?Mac#i9febc9b
;;
;; Apple Script to update PDF in Preview.app 2013-09-28 currently not active
;; ~/scripts/refresh-preview.scpt
;; tell application "Preview" to activate
;; tell application "Emacs" to activate
;;
;;; Add commands
;; 4.1.2 Selecting and Executing a Command
;; https://www.gnu.org/software/auctex/manual/auctex/Selecting-a-Command.html
;; %s becomes the file name without .tex, but .Rnw is not removed correctly.
;;
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (add-to-list 'TeX-command-list
                                   '("Preview" "/usr/bin/open -a Preview.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Preview"))
                      (add-to-list 'TeX-command-list
                                   '("Skim" "/usr/bin/open -a Skim.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Skim"))
                      ;; Replace original View
                      (add-to-list 'TeX-command-list
                                   '("View" "/usr/bin/open -a Skim.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Skim"))
                      ;;
                      (add-to-list 'TeX-command-list
                                   '("displayline" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %s.pdf \"%b\""
                                     TeX-run-discard-or-function t t :help "Forward search with Skim"))
		      ;; TeXShop for PDF
		      ;; Source - Configure for External Editor
		      ;; Preview - Automatic Preview Update
                      (add-to-list 'TeX-command-list
                                   '("TeXShop" "/usr/bin/open -a TeXShop.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run TeXShop"))
                      (add-to-list 'TeX-command-list
                                   '("TeXworks" "/usr/bin/open -a TeXworks.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run TeXworks"))
                      (add-to-list 'TeX-command-list
                                   '("Firefox" "/usr/bin/open -a Firefox.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Mozilla Firefox"))
                      (add-to-list 'TeX-command-list
                                   '("AdobeReader" "/usr/bin/open -a \"Adobe Reader.app\" %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Adobe Reader")))))
;;
;; (setq TeX-default-mode 'japanese-latex-mode)
;; jsarticle as the default style
(setq japanese-LaTeX-default-style "jsarticle")
;; For Japanese document
(setq kinsoku-limit 10)
;;
;; Engines
;; (setq TeX-engine-alist '((ptex "pTeX" "eptex" "platex" "eptex")
;;                          (uptex "upTeX" "euptex" "uplatex" "euptex")))
;; Select TeX engine from default(pdfTeX) luatex omega ptex uptex xetex
;; (setq TeX-engine 'ptex)
;;
;; Japanese setting by Dr. Okumura (not used for now
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX#h32722ec
;;
;; TeX-PDF mode (no DVI intermediate file in pdfTeX, LuaTex, XeTeX)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;;
;;; auctex-latexmk.el
;; for Japanese tex to PDF direct conversion
;; http://qiita.com/tm_tn/items/cbc813028d7f5951b165
;; https://github.com/tom-tan/auctex-latexmk/
;; ln -sf ~/Documents/.latexmkrc ~/.latexmkrc
;; C-c C-c LatexMk to use
(require 'auctex-latexmk)
(auctex-latexmk-setup)
;;
;; Interactive mode for errors
(add-hook 'LaTeX-mode-hook 'TeX-interactive-mode)


;;;
;;; Auto-completion for LaTeX
;;
;;; auto-complete-auctex.el
;; This is faster than ac-math.el. 2015-09-06
;; https://github.com/monsanto/auto-complete-auctex
;; Needs ac to be loaded first
;; (require 'auto-complete)
;; (require 'auto-complete-auctex)
;;
;;; ac-math.el
;; auto-complete sources for input of mathematical symbols and latex tags
;; https://github.com/vitoshka/ac-math#readme
;; (require 'ac-math)
;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
;; ;;
;; ;; Load by hook
;; (add-hook 'LaTeX-mode-hook (lambda ()
;; 			     ;; Configure these ac-math.el variables only
;; 			     (setq ac-sources '(ac-source-math-latex ac-source-latex-commands))
;; 			     ))
;;
;;; company-auctex.el
;; https://github.com/alexeyr/company-auctex
(require 'company-auctex)
(company-auctex-init)
;;
;;; company-math.el
;; https://github.com/vspinu/company-math
(require 'company-math)
;;
;; global activation of the unicode symbol completion
(add-to-list 'company-backends 'company-math-symbols-unicode)
;;
;; local configuration for TeX modes
(defun company-latex-mode-setup ()
  (company-mode)
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))
(add-hook 'TeX-mode-hook 'company-latex-mode-setup)


;;;
;;; latex-math-preview.el
;; http://www.emacswiki.org/emacs/LaTeXMathPreview
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
;; Paths to required external software (specific to MacTeX)
(setq latex-math-preview-command-path-alist
      '((latex    . "/Library/TeX/texbin/latex")
        (dvipng   . "/Library/TeX/texbin/dvipng")
        (dvips    . "/Library/TeX/texbin/dvips")
        ;; for beamer preview
        (pdflatex . "/Library/TeX/texbin/pdflatex")
        ;; for beamer preview
        (gs       . "/Library/TeX/local/bin/gs")
        ))
;; Colors for dark background 2013-09-28
(setq latex-math-preview-dvipng-color-option nil)
(setq latex-math-preview-image-foreground-color "black")
(setq latex-math-preview-image-background-color "white")
;;
;; Function to preview math expression and shift focus after preview
(defun my-latex-math-preview ()
  "Preview a math expression and shift focus after preview"
  (interactive)
  (setq w1 (selected-window))
  (latex-math-preview-expression)
  (select-window w1))
;;
;; Function to preview Beamer slide and shift focus after preview
(defun my-latex-beamer-preview ()
  "Preview a Beamer slide and shift focus after preview"
  (interactive)
  (setq w1 (selected-window))
  (latex-math-preview-beamer-frame)
  (select-window w1))
;;


;;;
;;; Reference management
;; http://en.wikibooks.org/wiki/LaTeX/Bibliography_Management
;; http://www.fan.gr.jp/~ring/doc/bibtex.html
;; http://d.hatena.ne.jp/ckazu/20100107/1262871971
;; http://stackoverflow.com/questions/144639/how-do-i-order-citations-by-appearance-using-bibtex
;;
;;; reftex.el
;; part of emacs
;; http://www.gnu.org/software/emacs/manual/html_mono/reftex.html
(require 'reftex)
;; turn on REFTeX mode by default
(add-hook 'LaTeX-mode-hook 'reftex-mode)
;; AUCTeX integration
(setq reftex-plug-into-AUCTeX t)
;; Do not prompt for reference vs page
(setq reftex-ref-macro-prompt nil)
;;
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (local-set-key (kbd "H-c") 'reftex-citation)
             (local-set-key (kbd "A-C-a") 'reftex-citation)))
;;
;;
;;; bibtex.el
;; http://en.wikibooks.org/wiki/LaTeX/Bibliography_Management#BibTeX
;; Getting current LaTeX document to use your .bib file
;; http://en.wikibooks.org/wiki/LaTeX/Bibliography_Management#Getting_current_LaTeX_document_to_use_your_.bib_file
(require 'bibtex)
;;
;;
;;; bibretrieve.el
;; Retrieving BibTeX
;; https://github.com/pzorin/bibretrieve; reftex 4.0 not found??
(require 'bibretrieve)
;;
;;
;;; bibtex-utils.el
;; Provides utilities for extending BibTeX
;; https://bitbucket.org/tws/bibtex-utils
(require 'bibtex-utils)
;;
;;
;;; zotelo (Zotero-Local)
;; https://github.com/vitoshka/zotelo
;; https://forums.zotero.org/discussion/19608/zotero-emacs-integration/
(require 'zotelo)
;;
;; (setq zotelo--auto-update-is-on t)
;; (setq zotelo-check-interval 600)
;; this variable is invalid and not refered to
;;
;; (setq zotelo-use-ido nil)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;; C-c z c         zotelo-set-collection (also C-c z s)
;; C-c z u         zotelo-update-database
;; C-c z e         zotelo-export-secondary
;; C-c z r         zotelo-reset
;; C-c z t         zotelo-set-translator
