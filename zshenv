fpath=($fpath $HOME/.zsh)
typeset -U fpath

export PATH=$PATH:$HOME/bin
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:/usr/texbin"
export MANPATH=$MANPATH:/usr/local/opt/erlang/lib/erlang/man
export NODE_PATH=/usr/local/lib/node_modules

# Privoxy log
export PRIVOXY_LOG="/usr/local/var/log/privoxy/logfile"

# OZ channels
export STOD2="71e448a6-34df-41c9-bcc4-7df545c926e0"
export KRTV="5c3c1b33-89f8-4ee3-bcd6-985195fd9076"
export GUSGUS="8c7c861e-f649-453a-a8fc-65612056a9ba"
export ASV="388392fe-d643-4bf1-a141-a45a084bc97b"
