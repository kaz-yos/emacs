;;; -*- lexical-binding: t; -*-

(use-package browse-url
  :config
  ;; Use xwidget when available and using GUI.
  ;; https://github.com/veshboo/emacs#example-customization-using-xwidget-webkit
  (when (and (display-graphic-p)
             (fboundp 'xwidget-webkit-browse-url)
             (featurep 'xwidget-internal))
    ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
    )
  ;;
  (when (eq system-type 'darwin)
    (defun browse-url-macosx-firefox-safari-browser (url &optional _new-window)
      "Invoke the macOS system's Firefox and Safari.
The optional NEW-WINDOW argument is not used."
      (interactive (browse-url-interactive-arg "URL: "))
      (start-process (concat "open-firefox" url) nil "open" "-a" "/Applications/Firefox.app" url)
      (start-process (concat "open-safari" url) nil "open" "-a" "/Applications/Safari.app" url))
    ;; (advice-add #'browse-url-default-macosx-browser
    ;;             :override
    ;;             #'browse-url-macosx-firefox-safari-browser)
    ))
