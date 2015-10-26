#!/usr/bin/env ruby

def ensure_directory_exists(filename)
  if !File.exists?(File.expand_path(filename))
    Dir::mkdir(File.expand_path(filename))
  end
end

file_links = {
  'Xmodmap' => '.Xmodmap',
  'abookrc' => '.abook/abookrc',
  'ackrc' => '.ackrc',
  'authinfo.gpg' => '.authinfo.gpg',
  'bashrc' => '.bashrc',
  'bashrc.d' => '.bashrc.d',
  'bin' => '.bin',
  'ctags' => '.ctags',
  'emacs.d' => '.emacs.d',
  'exelse' => '.exelse',
  'git' => '.git',
  'gitconfig' => '.gitconfig',
  'gitignore' => '.gitignore',
  'gitmessage' => '.gitmessage',
  'gnus' => '.gnus',
  'i3' => '.i3',
  'i3status.conf' => '.i3status.conf',
  'msmtprc' => '.msmtprc',
  'mutt' => '.mutt',
  'newsbeuter-config' => '.newsbeuter/config',
  'octaverc' => '.octaverc',
  'offlineimaprc' => '.offlineimaprc',
  'offlineimap.py' => '.offlineimap.py',
  'quicklisp' => '.quicklisp',
  'rspec' => '.rspec',
  'sbclrc' => '.sbclrc',
  'ssh-config' => '.ssh/config',
  'tmux.conf' => '.tmux.conf',
  'urlview' => '.urlview',
  'wallpaper' => '.wallpaper',
  'xbindkeysrc' => '.xbindkeysrc',
}

ensure_directory_exists("~/.ssh")
ensure_directory_exists("~/.newsbeuter")
ensure_directory_exists("~/.abook")

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
