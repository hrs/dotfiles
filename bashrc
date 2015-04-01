# Load any supplementary scripts
for config in "$HOME"/.bashrc.d/*.bash ; do
  source "$config"
done
unset -v config
