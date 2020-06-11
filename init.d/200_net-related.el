;;; -*- lexical-binding: t; -*-

(use-package browse-url
  :config
  ;; Use xwidget when available and using GUI.
  ;; https://github.com/veshboo/emacs#example-customization-using-xwidget-webkit
  (when (and (display-graphic-p)
             (fboundp 'xwidget-webkit-browse-url))
    (setq browse-url-browser-function 'xwidget-webkit-browse-url)))
