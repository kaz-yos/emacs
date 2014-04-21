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

This is like the "Start Menu". M-x helm-for-files, which I assigned C-z, shows current buffers and recently used files in one place. There are multiple plugins that make multiple aspects of daily emacs use very convenient.


icicles.el
-------------------------------------------------------------------------------

- Web Site: http://www.emacswiki.org/emacs/Icicles

- My Config: https://github.com/kaz-yos/emacs/blob/master/init.d/500_auto-completion-related.el


Little known, but powerful alternative to the ido-mode. It is an auto-completion plugin that makes \*Completion\* buffer more useful. The web site is too dense, and it is not easy to figure out how to configure it. With my configuration, 


magit.el
-------------------------------------------------------------------------------

- Web Site: 

- My Config: 
