;;; TeX environments
;;
;; AUCtex
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
(setq TeX-save-query nil) ;;autosave before compiling
;;
;; TeX-fold-mode on by default (C-c C-b C-o to actually fold
;; http://tex.stackexchange.com/questions/52179/what-is-your-favorite-emacs-and-or-auctex-command-trick
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (TeX-fold-mode 1)))
;;
;; Avoid font size changes 2013-10-14
;; http://stackoverflow.com/questions/9534239/emacs-auctex-latex-syntax-prevents-monospaced-font
;; Only change sectioning colour
(setq font-latex-fontify-sectioning 'color)
;; super-/sub-script on baseline
(setq font-latex-script-display (quote (nil)))
;; Do not change super-/sub-script font

;; Exclude bold/italic from keywords
(setq font-latex-deactivated-keyword-classes
    '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))
;;
;;
;; Japanese setting etc
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX
;; Hiragino font settings. Done in shell
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?Mac#i9febc9b
;;
;; Apple Script to update PDF in Preview.app 2013-09-28 currently not active
;; ~/scripts/refresh-preview.scpt
;; tell application "Preview" to activate
;; tell application "Emacs" to activate
;;
;; Add commands
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (add-to-list 'TeX-command-list
                                   '("Preview" "/usr/bin/open -a Preview.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Preview"))
                      (add-to-list 'TeX-command-list
                                   '("Skim" "/usr/bin/open -a Skim.app %s.pdf"
                                     TeX-run-discard-or-function t t :help "Run Skim"))
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
                                     TeX-run-discard-or-function t t :help "Run Adobe Reader"))
		      )))
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
;; auctex-latexmk.el for Japanese tex to PDF direct conversion:
;; http://qiita.com/tm_tn/items/cbc813028d7f5951b165
;; https://github.com/tom-tan/auctex-latexmk/
;; ln -sf ~/Documents/.latexmkrc ~/.latexmkrc
(require 'auctex-latexmk)
(auctex-latexmk-setup)
;;
;; Interactive mode for errors
(add-hook 'LaTeX-mode-hook 'TeX-interactive-mode)
;;
;; Single-step compilation that works with Japanese	; 2013-09-28 removed
;; http://miyazakikenji.wordpress.com/2013/07/10/ess-で-sweave-出力/
;; http://stackoverflow.com/questions/11060023/ess-auctex-sweave-synctex-integration-from-rnw-pdfviewer
;;
;;
;; Auto-complete for LaTeX
;;
;; ac-math.el: auto-complete sources for input of mathematical symbols and latex tags
;; https://github.com/vitoshka/ac-math#readme
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
;;
;; ac-latex-mode.el
(defun add-ac-source-words-in-same-mode-buffers () ; add back
  (setq ac-sources
        (append '(ac-source-words-in-same-mode-buffers)
                ac-sources))
  )
;;
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
	(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		ac-sources))
  )
;; Load by hook
(add-hook 'LaTeX-mode-hook (lambda ()
			     ;; For ac-latex-mode.el Overwrite default in LaTeX mode
			     (setq ac-sources '(ac-source-math-latex ac-source-latex-commands))
			     ;; (ac-l-setup) ; For auto-complete-latex (overwrite ac-sources)
			     ;; (ac-latex-mode-setup) ; For ac-math (add to ac-sources)
			     ;; Add back text completion.
			     ;; (add-ac-source-words-in-same-mode-buffers) ; Slow 2013-09-15 2013-10-12 not helpful
			     (local-set-key (kbd "M-p") 'ess-nuke-trailing-whitespace)
			     ))
;;
;; auto-complete-auctex.el ; 2014-02-23 now via ELPA
;; https://github.com/monsanto/auto-complete-auctex
;; (require 'auto-complete-auctex)
;;
;; ;; auto-complete-latex.el 2013-09-09. Not useful turned off on 2013-10-12
;; ;; (available on el-get but requires hg (mercurial)?)
;; ;; https://bitbucket.org/tequilasunset/auto-complete-latex/src
;; ;; http://d.hatena.ne.jp/tequilasunset/20100202/p1
;; ;; http://d.hatena.ne.jp/tequilasunset/20100317/p1
;; ;; http://d.hatena.ne.jp/whitypig/20110908/1315477128
;; ;; http://keisanbutsuriya.blog.fc2.com/blog-entry-59.html
;; ;; $ hg clone https://bitbucket.org/tequilasunset/auto-complete-latex # installed hg from brew
;; (require 'auto-complete-latex)
;; (setq ac-l-dict-directory "~/.emacs.d/auto-install/ac-l-dict/") ; Manually created here
;; ;; (add-to-list 'ac-modes 'foo-mode)
;; ;; (add-hook 'LaTeX-mode-hook 'ac-l-setup) ; 2013-09-10 Configured in ac-math settings
;; ;;
;; ;;       SYMBOL |           MEANING
;; ;;      --------+----------------------------------
;; ;;         l    | LaTeX or pLaTeX
;; ;;         a    | AMS packages
;; ;;         b    | beamer
;; ;;         h    | hyperlinks
;; ;;         g    | graphics
;; ;;         m    | math sign or equations
;; ;;         c    | colors
;; ;;         t    | tables
;; ;;         f    | fonts
;; ;;         p    | unclassified external packages
;; ;;         F    | file names in a current directory
;; ;;         L    | label names
;; ;;         B    | bib keys
;; ;;         u    | user-commands or user-arguments
;;
;;
;; latex-math-preview.el 2013-09-08
;; http://www.emacswiki.org/emacs/LaTeXMathPreview
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
;; Paths to required external software (specific to MacTeX)
(setq latex-math-preview-command-path-alist
      '((latex . "/usr/texbin/latex")
	(dvipng . "/usr/texbin/dvipng")
	(dvips . "/usr/texbin/dvips")
	(pdflatex . "/usr/texbin/pdflatex")	; for beamer preview
	(gs . "/usr/local/bin/gs")		; for beamer preview
	))
;; Colors for dark background 2013-09-28
(setq latex-math-preview-dvipng-color-option nil)
(setq latex-math-preview-image-foreground-color "black")
(setq latex-math-preview-image-background-color "white")
;; Function to preview math expression and shift focus after preview
(defun my-latex-math-preview ()
  "Preview a math expression and shift focus after preview"
  (interactive)
  (setq w1 (selected-window))
  (latex-math-preview-expression)
  (select-window w1)
  )
;; (define-key LaTeX-mode-map (kbd "C-c m") 'my-latex-math-preview)
;; Function to preview Beamer slide and shift focus after preview
(defun my-latex-beamer-preview ()
  "Preview a Beamer slide and shift focus after preview"
  (interactive)
  (setq w1 (selected-window))
  (latex-math-preview-beamer-frame)
  (select-window w1)
  )
(define-key LaTeX-mode-map (kbd "C-c s") 'my-latex-beamer-preview)
;;
;;
;; bibretrieve.el
;; https://github.com/pzorin/bibretrieve; reftex 4.0 not found??
;; (require 'reftex)
;; (require 'bibretrieve)
;;
;; zotelo (Zotero-Local)
;; https://github.com/vitoshka/zotelo for more
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;;
;;
;; YaTeX
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?YaTeX
