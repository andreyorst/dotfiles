# .dotfiles
This repository is mostly for maintaining my [Emacs][1] configuration, although it includes some useful scripts and settings for different Linux-related tools.

The structure of this repository is pretty simple.
Each directory that is named along with the default system name contains files that should be copied or sym-linked to the corresponding system directory.
All hidden files at the root of the repository except `.gitignore` should be copied or sym-linked to the home directory.

There's a `scripts/init` script that automatically links all needed files to their destinations, though you might want to look into it and possibly edit it.


[1]: .config/emacs

<!--  LocalWords:  sym dotfiles
 -->
