if [ -e /opt/homebrew/opt/chruby/share/chruby ]; then
  source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
  source /opt/homebrew/opt/chruby/share/chruby/auto.sh
elif [ -e /usr/local/share/chruby ]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh
else
  echo "Can't load chruby config files! Is it installed?"
fi

chruby 3.4.2
