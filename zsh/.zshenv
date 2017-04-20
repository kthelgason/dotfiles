fpath=($fpath $HOME/.zsh)
typeset -U fpath

# Privoxy log
export PRIVOXY_LOG="/usr/local/var/log/privoxy/logfile"

export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
export GREP_OPTIONS="--color"

export PATH=$PATH:$HOME/bin
export PATH="/usr/local/heroku/bin:$PATH"
export PATH=$HOME/bin/depot_tools:$PATH

# Highlight ack matches
export ACK_COLOR_MATCH='red'

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

export EDITOR="$HOME/homebrew/bin/emacsclient -t"
export VISUAL="view"

