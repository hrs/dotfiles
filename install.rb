#!/usr/bin/env ruby

file_links = {
  'Xmodmap' => '.Xmodmap',
  'ackrc' => '.ackrc',
  'authinfo.gpg' => '.authinfo.gpg',
  'bashrc' => '.bashrc',
  'bashrc.aliases' => '.bashrc.aliases',
  'bashrc.prompt' => '.bashrc.prompt',
  'bashrc.utils' => '.bashrc.utils',
  'bin' => '.bin',
  'ctags' => '.ctags',
  'emacs.d' => '.emacs.d',
  'exelse' => '.exelse',
  'git' => '.git',
  'gitconfig' => '.gitconfig',
  'gitignore' => '.gitignore',
  'gnus' => '.gnus',
  'octaverc' => '.octaverc',
  'offlineimaprc' => '.offlineimaprc',
  'offlineimap.py' => '.offlineimap.py',
  'quicklisp' => '.quicklisp',
  'rspec' => '.rspec',
  'sbclrc' => '.sbclrc',
  'ssh-config' => '.ssh/config',
  'tmux.conf' => '.tmux.conf',
  'wallpaper' => '.wallpaper',
  'xinitrc' => '.xinitrc'
}

unless File.exists?(File.expand_path('~/.ssh'))
  Dir::mkdir(File.expand_path('~/.ssh'))
end

file_links.each do |source, target|
  target_file = File.expand_path("~/#{target}")
  source_file = File.expand_path("~/.dotfiles/#{source}")

  unless File.exists?(target_file) || File.symlink?(target_file)
    File.symlink(source_file, target_file)
  end
end

if File.exists?(File.expand_path('~/.bash_profile'))
  File.delete(File.expand_path('~/.bash_profile'))
end

File.symlink(File.expand_path('~/.bashrc'), File.expand_path('~/.bash_profile'))
