
for config_file ($HOME/.zsh/*); do
  source $config_file
done

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
export VISUAL="vim"

alias ta="tmux att"
alias tl="tmux ls"

# General aliases
alias update="brew update && brew upgrade"
alias gimme="brew install"
alias v="~/.v"
alias vi="mvim"
alias finder="open . &"
alias reload="source ~/.zshrc"
alias ea="sudo vim ~/.zshrc && reload"
alias tags="/usr/local/bin/tag --list `ls`"
alias tmuxhelp="cat /etc/tmux.cheat"

# Python/django
alias pyhton="python"
alias py="python3"
alias ipy="ipython3"
alias django="django-admin.py"
alias bake="python manage.py"

# Ruby/Rails
alias r="rails"

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle -e ':completion:*:approximate:*' \
        max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Terminal Options
setopt AUTO_CD           # implicate cd for non-commands
setopt CD_ABLE_VARS       # read vars in cd
setopt CORRECT            # correct spelling
setopt COMPLETE_IN_WORD       # complete commands anywhere in the word
setopt NOTIFY              # Notify when jobs finish
setopt C_BASES             # 0xFF
setopt BASH_AUTO_LIST      # Autolist options on repeition of ambiguous args
setopt CHASE_LINKS         # Follow links in cds
setopt AUTO_PUSHD          # Push dirs into history
setopt ALWAYS_TO_END       # Move to the end on complete completion
setopt LIST_ROWS_FIRST     # Row orientation for menu
setopt MULTIOS             # Allow Multiple pipes
setopt MAGIC_EQUAL_SUBST   # Expand inside equals
setopt EXTENDED_GLOB
setopt AUTO_PUSHD

bindkey -v
bindkey '\e[3~' delete-char
bindkey '^R' history-incremental-search-backward

unsetopt correctall

bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

set -o emacs

export PATH=$PATH:$HOME/bin

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

. `brew --prefix`/etc/profile.d/z.sh
