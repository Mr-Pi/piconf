if [ $ENVIRONMENT = 'erlang' ]
then
	echo "stop environment"

	unset APPNAME
	unset AUTOMODULE
	unset ENVIRONMENT

	unalias rs
	unalias vim
	
	tmux send-keys -t 1 C-c a
	tmux send-keys -t 2 C-c
else
	MAXWIDTH=`tput cols`
	RWIDTH=$((MAXWIDTH/3))

	tmux split-window -h 
	tmux resize-pane -x $RWIDTH
	tmux split-window 'while true; do rebar get-deps && hg status && sleep 2; done'
	tmux resize-pane -y 13
	tmux select-pane -L

	unset MAXWIDTH
	unset RWIDTH

	export APPNAME='piconf'
	export AUTOMODULE='piconf_mainServer'
	export ENVIRONMENT='erlang'
	
	alias rs="tmux send-keys -t 1 'halt().' C-m"
	alias vim="vim +\"map %R :!tmux send-keys -t 1 'halt().' C-m<CR><CR>\" +\"imap %R <ESC>%Ra\""
	
	vim +"map %R :!tmux send-keys -t 1 'halt().' C-m<CR><CR>" +"imap %R <ESC>%Ra"
fi
