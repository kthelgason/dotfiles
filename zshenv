fpath=($fpath $HOME/.zsh)
typeset -U fpath

export PATH=$PATH:$HOME/bin
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export MANPATH=$MANPATH:/usr/local/opt/erlang/lib/erlang/man
export NODE_PATH=$(/usr/local/bin/npm root -g)

# Source chruby script
source /usr/local/opt/chruby/share/chruby/chruby.sh

# OPAM configuration
#. /Users/kthelgason/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Privoxy log
export PRIVOXY_LOG="/usr/local/var/log/privoxy/logfile"
