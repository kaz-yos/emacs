;;; haskell-mode.el
;; https://github.com/haskell/haskell-mode#haskell-mode-for-emacs
(require 'haskell-mode)
;;
;;
;;; my-send-to-haskell
(defun my-send-to-haskell (start end)
  "Sends expression to *haskell* and have it evaluated."

  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change to Haskell shell
    (switch-to-haskell)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (comint-send-input)
    ;; One more time if not ending with \n
    (if (not (equal (substring region-string -1) "\n"))
	(comint-send-input))
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))
;;; my-haskell-eval
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_haskellel/
(defun my-haskell-eval ()
  "Evaluates Haskell expressions"
  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; defined in 200_my-misc-functions-and-bindings.el
    (my-repl-start "*haskell*" #'run-haskell)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(my-send-to-haskell (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to eol
      (end-of-line)
      ;; ;; Go to the end of statment
      ;; (haskell-nav-end-of-statement)	; nonexistent
      ;; ;; Go to the end of block
      ;; (haskell-nav-end-of-block)	; nonexistent
      ;; ;; Send region if not empty
      (if (not (equal (point) (mark)))
      	  (my-send-to-haskell (point) (mark))
      	;; If empty, deselect region
      	(setq mark-active nil))
      ;; ;; Move to the next statement
      ;; (haskell-nav-forward-statement)	; nonexistent
      
      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (switch-to-haskell)
      ;; Switch back to the script window
      (select-window w-script)					
      )))
;;
;; Define hooks
(add-hook 'haskell-mode-hook		; For Haskell script
          '(lambda()
	     (local-set-key (kbd "<C-return>") 'my-haskell-eval)
	     ))
