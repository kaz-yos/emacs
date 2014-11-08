;; M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ac-ispell-requires 4)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#042028")
 '(background-mode dark)
 '(coffee-args-compile (quote ("-c" "--bare")))
 '(coffee-tab-width 2)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (modified-manoj-dark-theme)))
 '(custom-safe-themes
   (quote
    ("696bcab946b2c00efb979beb008ddbc1dcdb9d217f99a8a80020bf52b991f58f" default)))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#2D2D2D")
 '(foreground-color "#708183")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(global-anzu-mode t)
 '(haskell-mode-hook
   (quote
    (turn-on-eldoc-mode turn-on-haskell-doc turn-on-haskell-indent)))
 '(linum-format " %7i ")
 '(magit-diff-options nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(vc-annotate-background "#202020")
 '(vc-annotate-color-map
   (quote
    ((20 . "#C99090")
     (40 . "#D9A0A0")
     (60 . "#ECBC9C")
     (80 . "#DDCC9C")
     (100 . "#EDDCAC")
     (120 . "#FDECBC")
     (140 . "#6C8C6C")
     (160 . "#8CAC8C")
     (180 . "#9CBF9C")
     (200 . "#ACD2AC")
     (220 . "#BCE5BC")
     (240 . "#CCF8CC")
     (260 . "#A0EDF0")
     (280 . "#79ADB0")
     (300 . "#89C5C8")
     (320 . "#99DDE0")
     (340 . "#9CC7FB")
     (360 . "#E090C7"))))
 '(vc-annotate-very-old-color "#E090C7"))
;;
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:foreground "red" :underline nil :weight extra-bold :height 1.0 :width expanded))))
 '(anzu-mode-line ((t (:foreground "dark blue" :weight ultra-bold :height 2.0))))
 '(company-scrollbar-bg ((t (:background "#999999"))))
 '(company-scrollbar-fg ((t (:background "#8c8c8c"))))
 '(company-tooltip ((t (:inherit default :background "#848484"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil)))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "gray90"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "PaleVioletRed1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "cyan1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "orange1"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "chartreuse1"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "IndianRed4"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "turquoise4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "tomato2"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red1" :weight ultra-bold :height 1.5)))))


;;; theme memo
;; DEFAULT
;;  adwaita -- Face colors similar to the default theme of Gnome 3 (Adwaita).
;;  deeper-blue -- Face colors using a deep blue background.
;;  dichromacy -- Face colors suitable for red/green color-blind users.
;;  light-blue -- Face colors utilizing a light blue background.
;;  manoj-dark -- Very high contrast faces with a black background.
;;  misterioso -- Predominantly blue/cyan faces on a dark cyan background.
;;  tango-dark -- Face colors using the Tango palette (dark background).
;;  tango -- Face colors using the Tango palette (light background).
;;  tsdh-dark -- Minor tweaks to the Emacs dark-background defaults.
;;  tsdh-light -- Minor tweaks to the Emacs white-background defaults.
;; *wheatgrass -- High-contrast green/blue/brown faces on a black background. (in use)
;;  whiteboard -- Face colors similar to markers on a whiteboard.
;;  wombat -- Medium-contrast faces with a dark gray background.
;;
;; 
;; OPTIONAL
;; soothe-theme: too dark, mode-line stoo small
;; birds-of-paradise-plus: too low contrast
;; 
