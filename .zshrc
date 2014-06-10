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
