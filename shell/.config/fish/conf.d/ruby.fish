alias be "bundle exec"
alias migrate "be rake db:migrate db:test:prepare"

if test -r ~/.asdf/asdf.fish
	source ~/.asdf/asdf.fish
end

asdf global ruby latest
