;;; PYTHON SUPPORT-RELATED
;;
;; python.el (in use):
;; http://caisah.info/emacs-for-python/
;;
;; python-mode.el (more features, more configurations):
;; http://www.janteichmann.me/projects/emacs_as_python_editor


;;;
;;; python.el
;; http://superuser.com/questions/345595/python-el-for-long-time-python-mode-el-user
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)
  ;;
  :config
  (setq python-shell-interpreter "~/anaconda/bin/python")
  ;;
  ;; ;; Default shell interaction commands
  ;; (define-key map (kbd "C-c C-p") 'run-python)
  ;; (define-key map (kbd "C-c C-s") 'python-shell-send-string)
  ;; (define-key map (kbd "C-c C-r") 'python-shell-send-region)
  ;; (define-key map (kbd "C-M-x")   'python-shell-send-defun)
  ;; (define-key map (kbd "C-c C-c") 'python-shell-send-buffer)
  ;; (define-key map (kbd "C-c C-l") 'python-shell-send-file)
  ;; (define-key map (kbd "C-c C-z") 'python-shell-switch-to-shell)
  ;;
  ;;
  ;; Define hooks
  ;; For Python script
  (add-hook 'python-mode-hook
            '(lambda()
               ;; Disable python-indent-dedent-line-backspace
               (local-unset-key (kbd "DEL"))
               ;; eldoc in the mode line. Slow? 2013-12-25
               ;; (eldoc-mode 1)
               ;; Indent after newline
               (local-set-key (kbd "C-m") 'newline-and-indent)
               ;; Check grammar
               (local-set-key (kbd "C-c c") 'python-check))))


;;;
;;; anaconda-mode.el
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :defer t)
(dolist (hook '(python-mode-hook inferior-python-mode-hook))
  (progn
    (add-hook hook 'anaconda-mode)
    (add-hook hook 'eldoc-mode)))
;;


;;;
;;; company-anaconda.el
;; https://github.com/proofit404/company-anaconda
(use-package company-anaconda
  :defer t)
(add-hook 'python-mode-hook
          '(lambda ()
             (add-to-list (make-local-variable 'company-backends)
                          'company-anaconda)))


;;;
;;; python-environment.el for virtualenv
;; Python virtualenv API for Emacs Lisp
;; https://github.com/tkf/emacs-python-environment
(use-package python-environment
  :defer t)


;; ;;;
;; ;;; pungi.el
;; (require 'pungi)
;; ;; This package provides integration with jedi virtualenv and buildout.
;; ;; When working within a virtualenv, configure python sys.path passed
;; ;; to `jedi:server-args' such jedi commands `jedi:complete',
;; ;; `jedi:goto-definition' and `jedi:doc' show the correct sources.
;; ;; Usage:
;; ;;   When you'd like project specific variables to be taken into account,
;; ;;   e.g python-mode specific changes, you can place a file at the root
;; ;;   of the project directory called .dir-locals.el, in which
;; ;;   you can set variables on a per-mode, or global basis.
;; ;;   See http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; ;;   for documentation.
;; ;;   Set the `pungi-setup-jedi' to a non-nil value in order for `jedi:setup' to
;; ;;   take those settings into account.
;; (setq pungi-setup-jedi t)
;; ;;   If jedi has been required, then jedi:setup will be triggered when
;; ;;   python-mode-hook is fired.



;; ;;;
;; ;;; jedi.el	; Python auto-completion for Emacs
;; ;; http://tkf.github.io/emacs-jedi/
;; ;; http://d.hatena.ne.jp/syohex/20140321/1395363772
;; ;;
;; ;; jedi:install-server
;; ;;   "This command installs Jedi server script jediepcserver.py in a
;; ;; Python environment dedicated to Emacs.  By default, the
;; ;; environment is at ``~/.emacs.d/.python-environments/default/``.
;; ;; This environment is automatically created by ``virtualenv`` if it
;; ;; does not exist.

;; ;; Run this command (i.e., type ``M-x jedi:install-server RET``)
;; ;; whenever Jedi.el shows a message to do so.  It is a good idea to
;; ;; run this every time after you update Jedi.el to sync version of
;; ;; Python modules used by Jedi.el and Jedi.el itself.

;; ;; You can modify the location of the environment by changing
;; ;; `jedi:environment-root' and/or `python-environment-directory'.  More
;; ;; specifically, Jedi.el will install Python modules under the directory
;; ;; ``PYTHON-ENVIRONMENT-DIRECTORY/JEDI:ENVIRONMENT-ROOT``.  Note that you
;; ;; need command line program ``virtualenv``.  If you have the command in
;; ;; an unusual location, use `python-environment-virtualenv' to specify the
;; ;; location.

;; ;; .. NOTE:: jediepcserver.py is installed in a virtual environment but it
;; ;;    does not mean Jedi.el cannot recognize the modules in virtual
;; ;;    environment you are using for your Python development.  Jedi
;; ;;    EPC server recognize the virtualenv it is in (i.e., the
;; ;;    environment variable ``VIRTUAL_ENV`` in your Emacs) and then
;; ;;    add modules in that environment to its ``sys.path``.  You can
;; ;;    also add ``--virtual-env PATH/TO/ENV`` to `jedi:server-args'
;; ;;    to include modules of virtual environment even you launch
;; ;;    Emacs outside of the virtual environment.

;; ;; .. NOTE:: It is highly recommended to use this command to install
;; ;;    Python modules for Jedi.el.  You still can install Python
;; ;;    modules used by Jedi.el manually.  However, you are then
;; ;;    responsible for keeping Jedi.el and Python modules compatible.

;; ;; See also:

;; ;; - https://github.com/tkf/emacs-jedi/pull/72
;; ;; - https://github.com/tkf/emacs-jedi/issues/140#issuecomment-37358527"

;; ;;
;; (require 'jedi)
;; (setq jedi:complete-on-dot t)				; binds . to jedi:dot-complete. Dot alone activates jedi
;; (add-hook 'python-mode-hook	     'jedi:setup)	; for full setup
;; (add-hook 'inferior-python-mode-hook 'jedi:setup)	; for full setup
;; ;; (add-hook 'python-mode-hook	     'jedi:ac-setup)	; for completion only. keys are not changed.
;; ;; (add-hook 'inferior-python-mode-hook 'jedi:ac-setup)	; for completion only. keys are not changed.
;; ;;
;; ;; C-c		Prefix Command
;; ;; .		jedi:dot-complete
;; ;; <C-M-tab>	jedi:complete
;; ;; <C-tab>		other-window-or-split
;; ;; C-c ,		jedi:goto-definition-pop-marker
;; ;; C-c .		jedi:goto-definition
;; ;; C-c /		helm-jedi-related-names
;; ;; C-c ?		jedi:show-doc
;; ;;
;; (add-hook 'jedi-mode-hook
;;           '(lambda()
;; 	     ;; (local-set-key (kbd "<C-M-tab>") 'jedi:complete)	; Assigned to Python major mode
;; 	     (define-key jedi-mode-map (kbd "<C-tab>") 'other-window-or-split)	; Assigned to Jedi minor mode
;; 	     (define-key jedi-mode-map (kbd "<C-M-tab>") 'jedi:complete)	; Assigned to Jedi minor mode
;; 	     ;; jedi:show-doc
;; 	     (define-key jedi-mode-map (kbd "C-c C-v") 'jedi:show-doc)		; Simulate ESS
;; 	     ))


;;;
;;; ein2.el	; Emacs IPython Notebook (EIN)
;; Forked version (different from the one by the original author on MELPA)
;; https://github.com/millejoh/emacs-ipython-notebook
;; Need to install from github
;; cd ~/.emacs.d/plugins/
;; git clone git://github.com/millejoh/emacs-ipython-notebook.git
;; Add the path. 2015-01-01 Now the forked version is ein on MELPA
;; (add-to-list 'load-path "~/.emacs.d/plugins/emacs-ipython-notebook/lisp")
;;
;; http://tkf.github.com/emacs-ipython-notebook/
;; Usage
;; Start IPython notebook server with $ ipython notebook --pylab inline
;; Hit M-x ein:notebooklist-open to open notebook list.
(use-package ein
  :commands (ein:connect-to-notebook)
  :config
  ;; Auto complete for ein
  (setq ein:use-auto-complete t)
  ;; Or, to enable "superpack" (a little bit hacky improvements):
  ;; (setq ein:use-auto-complete-superpack t)
  (add-hook 'ein:notebook-multilang-mode-hook
            '(lambda()
               (local-set-key (kbd "<C-return>") 'ein:worksheet-execute-cell-and-goto-next)
               (local-set-key (kbd "<S-return>") 'ein:worksheet-execute-cell)
               (local-set-key (kbd "C-c a")      'ein:worksheet-insert-cell-above)
               (local-set-key (kbd "C-c b")      'ein:worksheet-insert-cell-below)
               (local-set-key (kbd "C-c k")      'ein:worksheet-kill-cell)
               (local-set-key (kbd "C-c w")      'ein:worksheet-copy-cell)
               (local-set-key (kbd "C-c y")      'ein:worksheet-yank-cell)
               (local-set-key (kbd "C-c p")      'ein:worksheet-goto-prev-input)
               (local-set-key (kbd "C-c n")      'ein:worksheet-goto-next-input)
               (local-set-key (kbd "A-[")        'ein:worksheet-goto-prev-input)
               (local-set-key (kbd "A-]")        'ein:worksheet-goto-next-input))))



;;;
;;; hy-mode.el
;; Hy mode for Emacs
;; https://github.com/hylang/hy-mode
(use-package hy-mode
  :mode "\\.hy"
  :config
  ;; Activate auto-complete-mode
  (add-hook 'hy-mode-hook
            '(lambda ()
               (auto-complete-mode 1))))
