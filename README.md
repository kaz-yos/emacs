emacs configuration files
=====

These are my emacs configuration files. These are mainly geared toward interactive data analysis with R.

Here I list plugins that I found useful.


init-loader.el
-------------------------------------------------------------------------------

- Web Site: https://github.com/emacs-jp/init-loader

- My Config: https://github.com/kaz-yos/emacs/blob/master/init.el


This plugin allows isolating different parts of configuration in separate files. The files in the subdirectory are loaded automatically. An additional bonus is that even if a plugin fails to load it does not affect the load process of plugins configured in separate files. The failures are reported as


helm.el
-------------------------------------------------------------------------------

- Web Site: https://github.com/emacs-helm/helm

- My Config: https://github.com/kaz-yos/emacs/blob/master/init.d/500_helm-related.el

This is like the "Start Menu". M-x helm-for-files, which I assigned C-z, shows current buffers and recently used files in one place. The candidates can be narrowed down by typing parts of the target name separated by spaces. There are multiple plugins that make multiple aspects of daily emacs use very convenient.


icicles.el
-------------------------------------------------------------------------------

- Web Site: http://www.emacswiki.org/emacs/Icicles

- My Config: https://github.com/kaz-yos/emacs/blob/master/init.d/500_auto-completion-related.el


Little known, but powerful alternative to the ido-mode. It is an auto-completion plugin that makes \*Completion\* buffer more useful. The web site is too dense, and it is not easy to figure out how to configure it. With my configuration, when I need to complete in the minibuffer, I just press tab multiple times to cycle through candidates, C-n/p also cycles forward/backward. Typing a part of the name will narrow down the candidates.


magit.el
-------------------------------------------------------------------------------

- Web Site: https://github.com/magit/magit

- My Config: https://github.com/kaz-yos/emacs/blob/master/init.d/500_version-control-related.el

A must-have for all git users. It makes partially staging and commiting changes in a file interactive and very easy. Also looking at history, etc, are made more approacheable than in the terminal.


Emacs Speaks Statistics (ESS)
-------------------------------------------------------------------------------

- Web Site: http://ess.r-project.org

- My Config: https://github.com/kaz-yos/emacs/blob/master/init.d/500_ess-related.el

The main reason why I use emacs. Very nice frontend to statistical software. It is mainly for R, but also works for Julia. C-RET starts up the R interpreter, then send expressions to the interpreter to be executed.
