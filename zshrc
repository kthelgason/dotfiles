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

# General aliases
alias update="brew update && brew upgrade --all"
alias gimme="brew install"
alias v="~/.v"
alias vi="mvim"
alias v="view"
alias finder="open . &"
alias reload="source ~/.zshrc"
alias ea="vim ~/.zshrc && reload"
alias tags="/usr/local/bin/tag --list `ls`"
alias eip="curl icanhazip.com"
alias fuck='$(thefuck $(fc -ln -1))'
alias getarch="ssh -p 3022 localhost"
alias plog="cat $PRIVOXY_LOG"
alias findpid="ps aux | selecta | awk '{print \$2}'"
alias diff="colordiff -u"
alias lock="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
alias em="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias e='emacsclient -t'
alias ec='emacsclient -c'

# Python/django
alias pyhton="python"
alias py="python3"
alias ipy="ipython3"
alias django="django-admin.py"
alias bake="python manage.py"

# Ruby/Rails
alias r="rails"

# Node
alias mka="foreman run -e /Users/kthelgason/oz/core_z/environments/test.env mocha"


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
setopt BASH_AUTO_LIST      # Autolist options on repetion of ambiguous args
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

# OPAM configuration
. /Users/kthelgason/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#------------------------------------
# functions
#------------------------------------
function take {
    mkdir $1
    cd $1
}

function cdf {
    cd *$1*/
}

function lack {
    ag --group --color $* | less -r +k
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

function fancy-ctrl-z {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER="fg"
        zle accept-line
    else
        zle push-input
        zle clear-screen
    fi
}

zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# Vim client-server helpers

VI_SERVER="VIM"

function vs {
    mvim --servername $VI_SERVER --remote-silent-tab $*
}

# Use zsh vim completion for vs
compdef _vim vs

function sure {
    number=$RANDOM
    let "number %= 10"
    mpv $HOME/Music/RA2/Allied\ Move\ \($number\).wav &> /dev/null
}

function insert-selecta-path-in-command-line() {
    local selected_path
    # Print a newline or we'll clobber the old prompt.
    echo
    # Find the path; abort if the user doesn't select anything.
    selected_path=$(find * -type f | selecta) || return
    # Append the selection to the current command buffer.
    eval 'LBUFFER="$LBUFFER$selected_path "'
    # Redraw the prompt since Selecta has drawn several new lines of text.
    zle reset-prompt
}

# Create the zle widget
zle -N insert-selecta-path-in-command-line
bindkey "^S" "insert-selecta-path-in-command-line"

