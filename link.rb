#!/usr/bin/env ruby

dotfiles = "$HOME/.dotfiles"

file_links = {
  "Xmodmap" => ".Xmodmap",
  "ackrc" => ".ackrc",
  "bashrc" => ".bashrc",
  "bashrc.aliases" => ".bashrc.aliases",
  "bashrc.prompt" => ".bashrc.prompt",
  "bashrc.utils" => ".bashrc.utils",
  "bin" => ".bin",
  "ctags" => ".ctags",
  "emacs.d" => ".emacs.d",
  "exelse" => ".exelse",
  "git" => ".git",
  "gitconfig" => ".gitconfig",
  "gitignore" => ".gitignore",
  "gnus" => ".gnus",
  "octaverc" => ".octaverc",
  "quicklisp" => ".quicklisp",
  "rspec" => ".rspec",
  "sbclrc" => ".sbclrc",
  "ssh-config" => ".ssh/config",
  "tmux.conf" => ".tmux.conf",
  "wallpaper" => ".wallpaper",
  "xinitrc" => ".xinitrc"
}

file_links.each do |source, target|
  `ln -s #{dotfiles}/#{source} $HOME/#{target}`
end
