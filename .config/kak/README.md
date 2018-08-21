# Kakoune config

Fancy screenshot for you:
![kakoune.png](https://user-images.githubusercontent.com/19470159/44391197-e47d4780-a537-11e8-8f66-62764627fc2d.png)

Currently I'm configuring it, so here's nothing special yet.

## plug.kak

[plug.kak](https://github.com/andreyorst/dotfiles/blob/master/.config/kak/plug.kak) is a [vim-plug](https://github.com/junegunn/vim-plug)-like plugin manager implementation for Kakoune
I'm currently working at. It features downloading plugins in asyncronous
manner, by spawning git clone processes, and waiting till every is finished
without blocking UI.

Also it can update all installed plugins in the same way.

Currently it provides two commands: `plug-install` and `plug-update`

The syntax for defining plugins in kakrc is `plug githubusername/projectname`.
Since I haven't firured out proper format for kakoune plugins, I'd not recommend
using this plugin manager.
