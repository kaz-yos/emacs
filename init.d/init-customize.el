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
    ("696bcab946b2c00efb979beb008ddbc1dcdb9d217f99a8a80020bf52b991f58f" "75d0c2a696b3d9dbd23fffb7cf87b77d1eb1262cbbed34b5de06961f4c5e6ad3" "fa9870b6a20cde7270ee75807b584b3a4b6a03d1fa9c406e6aee76db8b3d7093" "a506f1eed1985783c767bce4e05ccfaf80e7b3a607c7d06426dd6f9475489483" "5249ce95362806f25c6928e567afbeac6955a502d17a21c6707f3926a40aeab8" "92573117ae8ed66c7bdfaf59682730cf6f8ce1d5480646e5f8d3958f8f5e27a2" "3164a65923ef23e0f3dff9f9607b4da1e07ef1c3888d0f6878feef6c28357732" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "c7cd81771525ff66c105413134cdf0330b0b5b88fd8096e5d56b0256872ba6c7" "4217c670c803e8a831797ccf51c7e6f3a9e102cb9345e3662cc449f4c194ed7d" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d" "f5db04080a5133bc99721d680a11cf974d60d1df347b08841b43c3e97f52d3bf" "fe0a47cc3952fede574527a1c28ddf3a1af381fc1fb5843ca60d22e4c841011a" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "1a093e45e4c3e86fa5ad1f8003660e7cda4d961cd5d377cee3fee2dad2faf19b" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "465be5317c7d95a84e376e095c21242f4f2ad75692ed806dcbb6fe27078260f1" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "30f861ee9dc270afc2a9962c05e02d600c998905433c8b9211dc2b33caa97c51" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "1c1e6b2640daffcd23b1f7dd5385ca8484a060aec901b677d0ec0cf2927f7cde" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "605080e40891cc991f53d3d9c79b427d18497d973a44fd12a86d2360429a6a3d" "865d6cb994f89c13b2d7e5961df4eabeea12494583c240c8fe9a788d0f4ee12c" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "d7f1c86b425e148be505c689fc157d96323682c947b29ef00cf57b4e4e46e6c7" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "8d584fef1225d72bfd32d7677ac7f281208140a2535ef0e9f46f0e76343f8aca" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "73b835431bdbc4e83a3b176a38ebb740fbac78aa2635e1d4827b3c8211e0bc99" "436dd3eb5ff5be80d2db88494b340fcf34dc70a715d19c5aa7b794b763ff0321" "e57e7b19da7b4cd0e5512d5e9bc20d31c9cf50112c462de15a76bce0ea3c5ef5" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "364a5e1aecdd0d24b70089050368851ea5ee593dc8cc6fb58cff1b8cfe88a264" default)))
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
