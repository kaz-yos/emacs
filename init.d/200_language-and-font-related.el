;;; Language and font settings


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
  (let ((size-ten (* (string-to-int size) 10)))
    (set-face-attribute 'default nil :height size-ten)))


;;;
;;; Mac OS X font settings
(when (eq system-type 'darwin)
  ;; Mac-only
;;; Japanese font setting that works
  ;; Cocoa Emacs Font Settings ; Best one so far. Good rescaling, and working with Greek letters φ phi
  ;; http://d.hatena.ne.jp/setoryohei/20110117
  ;;
  ;; default-frame font configured
  ;; New font set made, it is then chosen as default-frame-alist font.
  ;; Fontset made with crease-fontse-from-ascii-font
  ;; Font selected by family name, font-spec object made.
  ;;
  ;; Fontset made
  (let* ((fontset-name "myfonts")                   ; Fontset name
         (size         14)                          ; Font size one of [9/10/12/14/15/17/19/20/...]
         (asciifont    "Menlo")                     ; ascii font
         (jpfont       "Hiragino Maru Gothic ProN") ; Japanese font
         ;;
         (font         (format "%s-%d:weight=normal:slant=normal" asciifont size))
         (fontspec     (font-spec :family asciifont))
         (jp-fontspec  (font-spec :family jpfont))
         ;; Create a fontset from an ASCII font FONT.
         (fsn          (create-fontset-from-ascii-font font nil fontset-name)))
    ;;
    ;; (set-fontset-font NAME TARGET FONT-SPEC &optional FRAME ADD)
    ;; Modify fontset NAME to use FONT-SPEC for TARGET characters.
    ;;
    ;;                NAME TARGET                   FONT-SPEC
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2      jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201        jp-fontspec) ; Half-sized katakana
    (set-fontset-font fsn '(#x0080 . #x024F)        fontspec)    ; Latin with pronounciation marks
    (set-fontset-font fsn '(#x0370 . #x03FF)        fontspec)    ; Greek
    )
  ;; Fontset for default-frame
  (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
  ;; Relative size of different fonts
  (dolist (elt '(("Hiragino Maru Gothic ProN"        . 1.2) ; 2014-05-26 to match jpfont. 1.2 times more
                 ("^-apple-hiragino.*"       . 1.2)
                 (".*osaka-bold.*"       . 1.2)
                 (".*osaka-medium.*"       . 1.2)
                 (".*courier-bold-.*-mac-roman"      . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman"      . 0.9)))
    (add-to-list 'face-font-rescale-alist elt))
  ;; Fontset configured to default face. Fixes default-frame-alist being ignored at startup.
  (set-face-font 'default "fontset-myfonts")
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
  ;;;
;;; inline patch for Japanese IME (require inline patch to Emacs.app. No .el dependency)
  ;; Change to English in minibuffer
  ;; http://molekun.blogspot.com/2011/03/homebrewemacs233.html
  ;; http://blog.n-z.jp/blog/2013-11-12-cocoa-emacs-ime.html
  (when (fboundp 'mac-change-language-to-us)
    ;; Only when inline patch is installed 2014-01-19
    (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)))



;;; Windows font settings
(when (eq system-type 'windows-nt)
  ;; http://d.hatena.ne.jp/eggtoothcroc/20130102/p1
  (set-face-attribute 'default nil :family "MeiryoKe_Console" :height 105))
