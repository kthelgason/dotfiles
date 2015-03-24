for config_file ($HOME/.zsh/*); do
  source $config_file
done

zmodload -i zsh/complist
setopt PROMPT_SUBST
autoload -U promptinit
promptinit

alias ls='ls -G'
alias la="ls -laG"
alias ll='ls -lG'
export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
export GREP_OPTIONS="--color"

# Use C-x C-e to edit the current command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# Highlight ack matches
export ACK_COLOR_MATCH='red'

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

export EDITOR="vim"
export VISUAL="view"

alias ta="tmux att"
alias tl="tmux ls"

alias latexmk="latexmk -pvc -interaction=nonstopmode"

# rm to trash
alias rm="rmtrash"

# General aliases
alias update="brew update && brew upgrade"
alias gimme="brew install"
alias v="~/.v"
alias vi="mvim"
alias finder="open . &"
alias reload="source ~/.zshrc"
alias ea="sudo vim ~/.zshrc && reload"
alias tags="/usr/local/bin/tag --list `ls`"
alias eip="curl icanhazip.com"


# Python/django
alias pyhton="python"
alias py="python3"
alias ipy="ipython3"
alias django="django-admin.py"
alias bake="python manage.py"

# Ruby/Rails
alias r="rails"

# Completion 
unsetopt menu_complete
unsetopt flowcontrol
setopt auto_menu
setopt complete_in_word
setopt complete_aliases

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*' use-cache on
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*' completer _expand _complete _match _approximate
zstyle ':completion:*:default' list-colors ${(s.:.)LSCOLORS}

autoload -U compinit
compinit

# Terminal Options
setopt AUTO_CD             # implicate cd for non-commands
setopt CD_ABLE_VARS        # read vars in cd
setopt CORRECT             # correct spelling
setopt NOTIFY              # Notify when jobs finish
setopt C_BASES             # 0xFF
setopt BASH_AUTO_LIST      # Autolist options on repeition of ambiguous args
setopt CHASE_LINKS         # Follow links in cds
setopt AUTO_PUSHD          # Push dirs into history
setopt LIST_ROWS_FIRST     # Row orientation for menu
setopt MULTIOS             # Allow Multiple pipes
setopt MAGIC_EQUAL_SUBST   # Expand inside equals
setopt EXTENDED_GLOB
setopt AUTO_PUSHD

# History options
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

set -o emacs

export PATH=$PATH:$HOME/bin
export MANPATH=$MANPATH:/usr/local/opt/erlang/lib/erlang/man

# Source chruby script
source /usr/local/opt/chruby/share/chruby/chruby.sh

#------------------------------------
# functions
#------------------------------------
function take {
    mkdir $1
    cd $1
}

function agn {
    ag $1 | cut -d ':' -f 1 | sort -u
}

function zipdir {
    if [ "$1" = "" ]; then
        name="archive.zip"
    else
        name="$1"
    fi
    zip -r "$name" *
}

function play {
    mpv $1 &> /dev/null &
}

function new-post {
    touch $(date +%Y-%m-%d)-$1.md
}

. `brew --prefix`/etc/profile.d/z.sh

# OPAM configuration
. /Users/kthelgason/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true


### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
