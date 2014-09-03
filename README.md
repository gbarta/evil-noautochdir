evil-noautochdir
================

In Vim, there is an option autochdir. When on, Vim will change the current
working directory whenever you open a file, switch buffers, delete a buffer
or open/close a window. It will change to the directory containing the file
which was opened or selected.

I don't like this option, but fortunately it is not turned on by default.

Unfortunately, emacs changes the default-directory variable in a manner very
similar to autochdir.

This plugin provides an Evil implementation of the default Vim directory
behaviour, "set noautochdir". It only affects Evil ex commands while other emacs
functionality remains unaffected.

The plugin provides the following ex-style commands for directly interacting with
the current directory:

*   :cd
*   :pwd

You can see the difference between the emacs default directory and the Evil
current directory by contrasting the different values returned by :pwd and M-x pwd.
