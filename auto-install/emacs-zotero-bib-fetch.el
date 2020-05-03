;;; emacs-zotero-bib-fetch.el --- Manage Zotero collections from emacs
;;
;; Filename: emacs-zotero-bib-fetch.el
;; Author: Anders Johansson, based on zotelo by Spinu Vitalie
;; Maintainer: Anders Johansson
;; Copyright (C) 2011-2014, Anders Johansson and Spinu Vitalie
;; Created: 1 Jul 2014
;; Version: 1.2
;; URL: https://github.com/andersjohansson/emacs-zotero-bib-fetch
;; Keywords: zotero, emacs, reftex, bibtex, bibliography manager
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Emacs-zotero-bib-fetch helps you efficiently export and synchronize
;; local databases (bib) and [Zotero](http://www.zotero.org) collections directly
;; from emacs using the Zotero-better-bibtex plugin for zotero.
;;
;; Emacs-zotero-bib-fetch can be used in conjunction with any emacs mode but is primarily
;; intended for bibtex and RefTeX users.
;;
;; zotelo-mode-map lives on  C-c z prefix.
;;
;;   *Installation*
;;
;;   (add-hook 'TeX-mode-hook 'ezbf-minor-mode)
;;
;;  See https://github.com/andersjohansson/emacs-zotero-bib-fetch for more
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ezbf-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-czu" 'ezbf-update-database)
    (define-key map "\C-cze" 'ezbf-export-secondary)
    (define-key map "\C-czs" 'ezbf-set-collection)
    (define-key map "\C-czc" 'ezbf-set-collection)

    map))

;;;###autoload
(defvar ezbf--url-regexp "^http://localhost:[0-9]+/better-bibtex/.+")
;;;###autoload
(defvar zotero-collection-url "" "File local variable holding the url to the primary bibliography collection.")
;;;###autoload
(make-variable-buffer-local 'zotero-collection-url)
;;;###autoload
(put 'zotero-collection-url 'safe-local-variable
	 '(lambda (x) (string-match ezbf--url-regexp x)))

(defvar ezbf--verbose nil)
(defun ezbf-verbose ()
  "Toggle ezbf debug messages (all printed in *message* buffer)"
  (interactive)
  (message "ezbf verbose '%s'" (setq ezbf--verbose (not ezbf--verbose))))

(defun ezbf--message (str)
  (when ezbf--verbose
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (insert (format "\n ezbf message [%s]\n %s\n" (current-time-string) str)))))

(defgroup ezbf nil "Customization for Emacs-zotero-bib-fetch"
  :group 'convenience)


;;;###autoload
(define-minor-mode ezbf-minor-mode
  "ezbf minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, `ezbf-set-collection' prompts
for zotero collection and stores it as file local variable . To
manually update the BibTeX data base call
`ezbf-update-database'. The \"file_name.bib\" file will be
created with the exported zotero items. To specify the file_name
just insert insert \\bibliography{file_name} anywhere in the
buffer.

This mode is designed mainly for latex modes and works in
conjunction with RefTex, but it can be used in any other mode
such as org-mode.

The following keys are bound in this minor mode:

\\{ezbf-minor-mode-map}"
  nil
  " ez"
  'ezbf-minor-mode-map
  :group ezbf
  (if ezbf-minor-mode t
    ;; (progn

    ;;   (unless (timerp ezbf--check-timer)
    ;;     (setq ezbf--check-timer
    ;;           (run-with-idle-timer 5 ezbf-check-interval 'ezbf--check-and-update-all)))
    ;;   )
    ;; (unless
    ;;     (loop for b in (buffer-list)
    ;;           for is-ezbf-mode = (buffer-local-value 'ezbf-minor-mode b)
    ;;           until is-ezbf-mode
    ;;           finally return is-ezbf-mode)
    ;;   ;; if no more active ezbf mode, cancel the timer and kill the process
    ;;   (when (timerp ezbf--check-timer)
    ;;     (cancel-timer ezbf--check-timer)
    ;;     (setq ezbf--check-timer nil)
    ;;     (delete-process (ezbf--moz-process))
    ;;     (kill-buffer ezbf--moz-buffer)
    ;;     )
    ;;   )
    )
  )





;;;###autoload
(defun ezbf-export-secondary ()
  "Export zotero collection into  secondary BibTeX database.

Before export, ask for a secondary database and zotero collection
to be exported into the database. Secondary databases are those
in \\bibliography{file1, file2, ...}, except the first one.

Error occurs if there is only one (primary) file listed in
\\bibliography{...}.

Error if zotero collection is not found by MozRepl"
  (interactive)
  (let* ((files (ezbf--locate-bibliography-files))
	 (bibfile (cond
		   ((< (length files) 2)
                    (error "No secondary databases (\\bibliography{...} lists contain less than 2 files)."))
		   ((= (length files) 2)
		    (cadr files))
		   (t (ido-completing-read "File to update: " (cdr files) nil t))))
	 (collurl (ezbf-set-collection
                   (format "Export into '%s': " (file-name-nondirectory bibfile))
                   'no-update 'no-set)))
    (ezbf-update-database bibfile collurl)))



;;;###autoload
(defun ezbf-update-database (&optional bibfile url)
  "Update the primary BibTeX database associated with the current buffer.

Primary database is the first file in \\bibliography{file1, file2,
...}, list. If you want to export into a different file use
`ezbf-export-secondary'.

If BIBFILE is supplied, don't infer from \\bibliography{...} statement.

If URL is supplied, don't infer collection url from file local variables.

Throw an error if zotero collection has not been found by MozRepl"
  (interactive)
  (let ((bibfile (or bibfile
                     (car (ezbf--locate-bibliography-files))))
        (url (or url
                 (ezbf--get-local-collection-url)))
        (file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))

    (unless bibfile
      (setq bibfile file-name)
      (message "Using '%s' filename for export." file-name)
      )

    (if (string-match "\\.bib$" bibfile)
        (setq bibfile (expand-file-name bibfile))
      (setq bibfile (concat (expand-file-name bibfile) ".bib")))
    ;;TODO, nödvändigt?
    ;; (setq bib-last-change (nth 5 (file-attributes bibfile))) ;; nil if bibfile does not exist

    (setq bibfile (replace-regexp-in-string "\\\\" "\\\\"
                                            (convert-standard-filename bibfile) nil 'literal))
    ;; Add cygwin support.
    ;; "C:\\foo\\test.bib" workes with javascript.
    ;; while "/foo/test.bib" "C:\cygwin\foo\test.bib" and "C:/cygwin/foo/test.bib" don't
    ;; (when (eq system-type 'cygwin)
    ;;   (setq bibfile
    ;;         (replace-regexp-in-string
    ;;          "/" "\\\\\\\\" (substring
    ;;                          (shell-command-to-string (concat "cygpath -m '" bibfile "'")) 0 -1))))

    ;; If url not set before or passed
    (when (and (called-interactively-p 'any) (null url))
      (ezbf-set-collection "No collection url set, input one: " 'no-update)
      (setq url (ezbf--get-local-collection-url)))

    (unless (file-exists-p (file-name-directory bibfile))
      (error "Directory '%s' does not exist; create it first." (file-name-directory bibfile)))
    ;; (when check-zotero-change
    ;;   (set-time-zone-rule t)
    ;;   (with-current-buffer (moz-command (format ezbf--dateModified-js url))
    ;;     (goto-char (point-min))
    ;;     (when (re-search-forward ":MozOK:" nil t) ;; ingore the error it is  cought latter
    ;;       (setq zotero-last-change (date-to-time
    ;;                                 (buffer-substring-no-properties (point) (point-max))))
    ;;       )))
    (when url

      ;; (setq cstr (format ezbf--export-collection-js bibfile url (cadr translator)))
      ;; (ezbf--message (format "Executing command: \n\n (moz-command (format ezbf--export-collection-js '%s' %s))\n\n translated as:\n %s\n"
      ;; 		       bibfile url cstr))
      (message "Updating '%s' ..." (file-name-nondirectory bibfile))
      ;; (setq com (split-string cstr "//split" t))
      ;; (while (setq com1 (pop com))
      ;; 	(when com ;; append to all except the last one
      ;; 	  (setq com1 (concat com1 "\":MozOk:\"")))
      ;; 	(with-current-buffer (moz-command com1)
      ;; 	  (goto-char (point-min))
      ;; 	  (unless (re-search-forward ":MozOK:" nil t)
      ;; 		(error "MozError: \n%s" (buffer-substring-no-properties (point) (point-max))))
      ;;     ))
      (url-copy-file url bibfile t)
      ;;TODO, error checking?
      (let ((buf (get-file-buffer bibfile)))
        (when buf (with-current-buffer buf (revert-buffer 'no-auto 'no-conf))))
      (message "'%s' updated successfully" (file-name-nondirectory bibfile))
      url)))

(defcustom ezbf-bibliography-commands '("bibliography" "nobibliography" "ezbf" "addbibresource")
  "List of commands which specify databases to use.

For example \\bibliography{file1,file2} or \\ezbf{file1,file2}
both specify that file1 is a primary database and file2 is the
secondary one.
"
  :group 'ezbf
  :type 'list)


(defun ezbf--locate-bibliography-files ()
  ;; Scan buffer for bibliography macro and return as a list.
  ;; Modeled after the corresponding reftex function

  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         (concat
                                        ;           "\\(\\`\\|[\n\r]\\)[^%]*\\\\\\("
          "\\(^\\)[^%\n\r]*\\\\\\("
          (mapconcat 'identity ezbf-bibliography-commands "\\|")
          "\\){[ \t]*\\([^}]+\\)") nil t)
        (split-string   (when (match-beginning 3)
                          (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
                        "[ \t\n\r]*,[ \t\n\r]*"))))


(defvar ezbf-coll-url-hist '())

;;;###autoload
(defun ezbf-set-collection (&optional prompt no-update no-file-local)
  "Ask for a zotero bib-collection url from Better-biblatex. This
is just input as a string and saved as a file local variable.

If no-update is t, don't update after setting the collecton.

If no-file-local is non-nil don't set file-local variable.

Return collection url.
"
  (interactive)
  (let* ((guessurl (ezbf--guess-collection-url)))
	(when guessurl (add-to-list 'ezbf-coll-url-hist guessurl))
	(let ((collurl
		 (read-from-minibuffer (or prompt "Zotero collection url: ")
							   nil nil nil
							   (cons ezbf-coll-url-hist 1)
							   guessurl)))
	  (if (or (null collurl) (string= "" collurl))
		  (error "No collection url entered")
		(unless no-file-local
		  (save-excursion
			(add-file-local-variable 'zotero-collection-url
									 collurl)
			(hack-local-variables))
		  (unless no-update
			(ezbf-update-database)))
		collurl))))

(defun ezbf--guess-collection-url ()
  (let ((xs (x-get-selection-value)))
	(when (and xs (string-match ezbf--url-regexp xs))
		xs)))

(defun ezbf--get-local-collection-url ()
  (cdr (assoc 'zotero-collection-url file-local-variables-alist)))

(provide 'emacs-zotero-bib-fetch)
;;; emacs-zotero-bib-fetch.el ends here.
