#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

### Completioning ###
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# enable exercism completion:
if [ -f ~/.config/exercism/exercism_completion.bash ]; then
    . ~/.config/exercism/exercism_completion.bash
fi

# load local bash completions
if [ -d ~/.local/etc/bash_completion.d ]; then
    for f in ~/.local/etc/bash_completion.d/*; do
        . $f
    done
fi

export EDITOR=$EMACS_TERM_CLIENT
export VISUAL=$EMACS_TERM_CLIENT
export BROWSER='chromium'

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

set_prompt(){
    Last_Command=$? # Must come first!
    Blue='\[\e[01;34m\]'
    White='\[\e[01;37m\]'
    Red='\[\e[01;31m\]'
    Green='\[\e[01;32m\]'
    Orcam1='\[\e[00;36m\]'
    Orcam2='\[\e[01;33m\]'
    Reset='\[\033\e[0m\]'
    FancyX='\342\234\227'
    Checkmark='\342\234\223'
    NoColor='\[\e[0m\]'


    PS1=""
    # If it was successful, print a green check mark. Otherwise, print
    # a red X.
    if [[ $Last_Command == 0 ]]; then
        PS1="$Green$Checkmark "
	      PS1XTERM="${Green}V "
    else
        PS1="$Red$FancyX "
	      PS1XTERM="${Red}X "
    fi

    export PS1="${PS1}${White}"'[\t] [\[\033[0;31m\]\u@\h\[\033[00m\]] [\[\033[1;36m\]\w\[\033[00m\]] $(parse_git_branch)\n\[\033[1;32m\]$ \[\033[00m\]'

}

export PROMPT_COMMAND="set_prompt"

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000000
HISTFILESIZE=2000000
HISTFILE=/home/$USER/.histfile


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# colored GCC warnings and errors
# export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'



### NVM ###
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


### Python virtualenv ###
export WORKON_HOME=~/Envs
source ~/.local/bin/virtualenvwrapper.sh
