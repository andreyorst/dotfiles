# .dotfiles
I really love to configure text editors.  [Kakoune][1] and [Emacs][2]
configurations are stored in this repository. There also was NeoVim config, but
since I'm no longer using and maintaining its configuration, it was removed. If
you're interested in it, you can browse the repository at [this commit][3].

Also includes settings for Termite, Tmux, and other misc stuff, that is usual
for Linux.

## Structure
The structure of this repository is pretty simple. Each directory that is named
along with default system name contains files that should be copied or
sym-linked to corresponding system directory. All hidden files at the root of
the repository should be copied of sym-linked to home directory.

```
.dotfiles               ~> /home/$USER
├── .bashrc             ~> ├── .bashrc
├── .config/*           ~> ├── .config/
├── .config/emacs       ~> ├── .emacs.d (for pre Emacs 27 users)
├── c_project_template     │
├── .editorconfig       ~> ├── .editorconfig
├── .git/                  │
├── .gitconfig          ~> ├── .gitconfig
├── .gitignore             │
├── .inputrc            ~> ├── .inputrc
├── README.md              │
├── scripts                │
└── .tmux.conf          ~> └── .tmux.conf
```

These directories and files should be excluded from copying or sym-linking:

- `.git`, `.gitignore`, `README.md` - obviously, there's no need to copy these
  files to your home directory, as those are git related files.
- `c_project_template` - is a template for creating a C project, that is used
  by a script, that is placed in `scripts` directory.
- `scripts` - directory stores scripts that I use directly from there, either by
  sourcing or by invoking from `$PATH`.
- `.config` directory should not be sym-linked to home directory, since all your
  configuration files will be visible to git. Only files that inside this
  directory should be copied or sym-linked.

[1]: .config/kak
[2]: .config/emacs
[3]: https://github.com/andreyorst/dotfiles/tree/58b56c0b7b2ff255b6cebf3ef1300bb632444155/.config/nvim
