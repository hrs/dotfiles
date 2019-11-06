### hrs's dotfiles

I hope you like fiddling with your `.emacs.d` ad nauseam, 'cause I obviously do.

#### Installation

Clone this thing wherever you like (I use `~/.dotfiles`), install [GNU
`stow`][], and run the `install.sh` script. That will:

- Install a bunch of Debian packages,
- Set up a bunch of symlinks in your home directory (e.g., `~/.bashrc` →
  `~/.dotfiles/bash/.bashrc`), and
- Install a handful of required Ruby gems.

[GNU `stow`]: https://www.gnu.org/software/stow/

It (mostly) won't overwrite existing files, so move those out of the way first.

This isn't *really* intended for anyone's use but my own, and it's catered to my
way of doing things (duh), so, you know, be prepared for that.

Enjoy! =)

#### Wallpaper

To ensure that wallpaper is set correctly, create a `~/.wallpaper-directory`
symlink to the directory where you keep your wallpapers:

``` shell
$ ln -s /where/your/wallpapers/live ~/.wallpaper-directory
```

#### Contributing

I'm open to non-malicious contributions. Shoot me a pull request, yo.
