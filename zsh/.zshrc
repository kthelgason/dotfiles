# -*- mode: sh -*-
for config_file ($HOME/.zsh/*) do
    source $config_file
done

zmodload -i zsh/complist
setopt PROMPT_SUBST
autoload -U promptinit
promptinit

alias ls='ls -G'
alias la="ls -laG"
alias ll='ls -lG'

# Use C-x C-e to edit the current command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

alias ta="tmux att"
alias tl="tmux ls"

alias latexmk="latexmk -pvc -interaction=nonstopmode"

# General aliases
alias update="brew update && brew upgrade --all"
alias gimme="brew install"
alias vi="$EDITOR"
alias v="view"
alias finder="open . &"
alias reload="source ~/.zshrc"
alias ea="$EDITOR ~/.zshrc && reload"
alias tags="/usr/local/bin/tag --list `ls`"
alias eip="curl icanhazip.com"
alias fuck='$(thefuck $(fc -ln -1))'
alias getarch="ssh -p 3022 -A localhost"
alias plog="cat $PRIVOXY_LOG"
alias findpid="ps aux | selecta | awk '{print \$2}'"
alias diff="colordiff -u"
alias lock="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
alias gti='git'
alias ec='emacsclient'
alias et='emacsclient -t'

# Python/django
alias pyhton="python"
alias py="python3"
alias ipy="ipython3"
alias django="django-admin.py"
alias bake="python manage.py"

# Ruby/Rails
alias r="rails"

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

# Make sure homebrew/bin is first in PATH
export PATH=$HOME/brew/bin:$PATH

# OPAM configuration
# . /Users/kthelgason/.opam/opam-init/init.zsh

# Source chruby script
#source /usr/local/opt/chruby/share/chruby/chruby.sh
#chruby ruby

# Z
. `brew --prefix`/etc/profile.d/z.sh

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

function up-dir {
    BUFFER=$(echo $BUFFER | rev | cut -d '/' -f 2- | rev)
}
zle -N up-dir
bindkey '\C-g' up-dir

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

function weather {
    curl "http://wttr.in/$1"
}

function search-replace-in-folder {
    local search=$1
    local replace=$2
    local folder=$3
    for file in $(ag -l $1 $3); do
        echo "Processing file ${file}"
        sed -i "" "s/${search}/${replace}/g" $file
    done
}

# Make pbcopy/paste work as expected in tmux
function _wrap {
    if [ -n "$TMUX" ]; then
        reattach-to-user-namespace $1
    else
        $1
    fi
}
alias pbcopy="_wrap pbcopy"
alias pbpaste="_wrap pbpaste"



# Create the zle widget
zle -N insert-selecta-path-in-command-line
bindkey "^S" "insert-selecta-path-in-command-line"

# The next line updates PATH for the Google Cloud SDK.
if [ -f /Users/kthelgason/google-cloud-sdk/path.zsh.inc ]; then
  source '/Users/kthelgason/google-cloud-sdk/path.zsh.inc'
fi

# The next line enables shell command completion for gcloud.
if [ -f /Users/kthelgason/google-cloud-sdk/completion.zsh.inc ]; then
  source '/Users/kthelgason/google-cloud-sdk/completion.zsh.inc'
fi

