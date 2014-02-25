init-loader directory
=========================

Numbered files in this folder is loaded by init-loader.el from the main init.el file.

- ../init.el: The main file should only configure the package systems and init-loader.el
- 000: Reserved for package systems and M-x customize system
- 050: Reserved for decent defaults
- 100: 
- 200: Usual place for files without package dependencies
- 300: 
- 400: 
- 500: Usual place for files with package dependencies
- 600: 
- 700: 
- 800: 
- 900: Use for configurations that have to come last
- init-customize.el: Actual configurations of M-x customize system go in this file

```
     Status: Installed in `/Users/kazuki/.emacs.d/elpa/init-loader-20140216.2240/'.
    Version: 20140216.2240
    Summary: Loader for configuration files

Place init-loader.el somewhere in your `load-path'.  Then, add the
following lines to ~/.emacs or ~/.emacs.d/init.el:

    (require 'init-loader)
    (init-loader-load "/path/to/init-directory")

The last line loads configuration files in /path/to/init-directory.
If you omit arguments for `init-loader-load', the value of
`init-loader-directory' is used.

Note that not all files in the directory are loaded.  Each file is
examined that if it is a .el or .elc file and, it has a valid name
specified by `init-loader-default-regexp' or it is a platform
specific configuration file.

By default, valid names of configuration files start with two
digits.  For example, the following file names are all valid:
    00_util.el
    01_ik-cmd.el
    21_javascript.el
    99_global-keys.el

Files are loaded in the lexicographical order.  This helps you to
resolve dependency of the configurations.

A platform specific configuration file has a prefix corresponds to
the platform.  The following is the list of prefixes and platform
specific configuration files are loaded in the listed order after
non-platform specific configuration files.

Platform   Subplatform        Prefix         Example
------------------------------------------------------------------------
Windows                       windows-       windows-fonts.el
           Meadow             meadow-        meadow-commands.el
------------------------------------------------------------------------
Mac OS X   Carbon Emacs       carbon-emacs-  carbon-emacs-applescript.el
           Cocoa Emacs        cocoa-emacs-   cocoa-emacs-plist.el
------------------------------------------------------------------------
GNU/Linux                     linux-         linux-commands.el
------------------------------------------------------------------------
All        Non-window system  nw-            nw-key.el

If `init-loader-byte-compile' is non-nil, each configuration file
is byte-compiled when it is loaded.  If you modify the .el file,
then it is recompiled next time it is loaded.

Loaded files and errors during the loading process are recorded.
If `init-loader-show-log-after-init' is non-nil, the record is
shown after the overall loading process.  You can do this manually
by M-x init-loader-show-log.
```
