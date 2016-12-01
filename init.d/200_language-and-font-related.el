;;; Language and font settings
;;
;; EmacsWiki SetFonts (including Testing if fonts are available?)
;; https://www.emacswiki.org/emacs/SetFonts
;; Xah Emacs: Set Font
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html


;;;
;;; Unicode use
;; http://d.hatena.ne.jp/syou6162/20080519/1211133695
(set-locale-environment "utf-8")
(setenv "LANG" "en_US.UTF-8")
;; (setenv "LANG" "ja_JP.UTF-8")
;; http://www.emacswiki.org/emacs/EmacsForMacOS#toc18
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;;
;;; Smooth Japanese input (Mac only?)
;; http://suzukima.hatenablog.com/entry/2012/08/16/232210
(setq show-paren-delay 0.25)


;;;
;;; Default font size setter (effective in all buffers)
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; http://ergoemacs.org/emacs/elisp_idioms_prompting_input.html
(defun default-font-size (size)
  "Set default font size (effective in all buffers)"
  (interactive "sEnter font size in points: ")
  (let ((ht (* (string-to-int size) 10)))
    (set-face-attribute 'default nil :height ht)))


;;;
;;; Mac OS X font settings
(when (eq system-type 'darwin)
  ;; Mac-only
;;; Japanese font setting that works
  ;; Best one so far. Good rescaling, and working with Greek letters φ phi
  ;; http://d.hatena.ne.jp/setoryohei/20110117
  ;; Good overview of how to configure a multi-language environment
  ;; http://qiita.com/melito/items/238bdf72237290bc6e42
  ;; Overview of an alternative method
  ;; http://lioon.net/emacs-change-font-size-quickly
  ;;
  ;; Set default-frame font via default-frame-alist
  ;;
  ;; Create a customized font set
  (let* ((my-fontset-name "myfonts")
         ;; Font size one of [9/10/12/14/15/17/19/20/...]
         (my-default-font-size 14)
         ;;
         ;; Ascii font name (pick from (font-family-list))
         (my-ascii-font "Menlo")
         ;; Japanese font name (pick from (font-family-list))
         (my-jp-font    "Hiragino Maru Gothic ProN")
         ;;
         ;; Create a FONT string for create-fontset-from-ascii-font
         ;; "Menlo-14:weight=normal:slant=normal"
         (my-default-font-string (format "%s-%d:weight=normal:slant=normal" my-ascii-font my-default-font-size))
         ;;
         ;; Create :family-only font specifications (use later)
         ;; #<font-spec nil nil Menlo nil nil nil nil nil nil nil nil nil nil>
         (my-ascii-fontspec (font-spec :family my-ascii-font))
         ;; #<font-spec nil nil Hiragino\ Maru\ Gothic\ ProN nil nil nil nil nil nil nil nil nil nil>
         (my-jp-fontspec    (font-spec :family my-jp-font))
         ;;
         ;; Create a fontset from an ASCII font FONT.
         ;; Name as "fontset-" added to my-fontset-name
         ;; "-*-menlo-normal-normal-normal-*-*-140-*-*-m-0-fontset-myfonts"
         (my-font-set (create-fontset-from-ascii-font my-default-font-string nil my-fontset-name)))
    ;;
    ;; set-fontset-font
    ;; Modify fontset NAME to use FONT-SPEC for TARGET characters.
    ;; (set-fontset-font NAME TARGET FONT-SPEC &optional FRAME ADD)
    ;;
    ;; NAME is a fontset name string, nil for the fontset of FRAME,
    ;; or t for the default fontset.
    ;;
    ;; TARGET may be a cons (FROM . TO) or a charset or others.
    ;; To list all possible choices, use M-x list-character-sets
    ;;
    ;; For these Japanese character sets, use my-jp-fontspec
    (set-fontset-font my-font-set 'japanese-jisx0213.2004-1 my-jp-fontspec)
    (set-fontset-font my-font-set 'japanese-jisx0213-2      my-jp-fontspec)
    ;; For Half-sized katakana characters, use my-jp-fontspec
    (set-fontset-font my-font-set 'katakana-jisx0201        my-jp-fontspec)
    ;;
    ;; For the characters in the range #x0080 - #x024F, use my-ascii-fontspec
    ;; Latin with pronounciation annotations
    (set-fontset-font my-font-set '(#x0080 . #x024F)        my-ascii-fontspec)
    ;; For the characters in the range #x0370 - #x03FF, use my-ascii-fontspec
    ;; Greek characters
    (set-fontset-font my-font-set '(#x0370 . #x03FF)        my-ascii-fontspec))
  ;;
  ;; Set the font set for the default frame (Used at frame creation)
  (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
  ;;
  ;; Rescaling parameters to adjust font sizes to match each other
  (dolist (elt '(("Hiragino Maru Gothic ProN"        . 1.2)
                 ("^-apple-hiragino.*"               . 1.2)
                 (".*osaka-bold.*"                   . 1.2)
                 (".*osaka-medium.*"                 . 1.2)
                 (".*courier-bold-.*-mac-roman"      . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman"       . 0.9)))
    ;; Alist of fonts vs the rescaling factors.
    (add-to-list 'face-font-rescale-alist elt))
  ;;
  ;; There is a bug relating to this rescaling setting.
  ;; https://debbugs.gnu.org/db/17/1785.html
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2014-12/msg00774.html
  ;;
  ;; /lisp/startup.el.gz line 672-
  ;; FIXME: The user's init file may change
  ;; face-font-rescale-alist.  However, the default face
  ;; already has an assigned font object, which does not take
  ;; face-font-rescale-alist into account.  For such
  ;; situations, we ought to have a way to find all font
  ;; objects and regenerate them; currently we do not.  As a
  ;; workaround, we specifically reset te default face's :font
  ;; attribute here.  See bug#1785.
  ;;
  ;; Examples
  ;; |123456 123456|
  ;; |Kazuki Yoshid|
  ;; |αβγδεζ ηθικλμ|
  ;; |ΑΒΓΔΕΖ ΗΘΙΚΛΜ|
  ;; |'";:-+ =/\~`?|
  ;; |日本語 の美観|
  ;; |よしだ かずき|
  ;; |ヨシダ カズキ|
  ;;
  )



;;; Windows font settings
;; http://qiita.com/melito/items/238bdf72237290bc6e42
(when (eq system-type 'windows-nt)
  ;; http://d.hatena.ne.jp/eggtoothcroc/20130102/p1
  (set-face-attribute 'default nil :family "MeiryoKe_Console" :height 140)
  ;;
  ;; For Japanese file names on Windows
  (setq default-file-name-coding-system 'shift_jis))
