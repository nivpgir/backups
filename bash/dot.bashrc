
#
# ~/.bashrc
#

# set -x
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



# load local bash completions
if [ -d ~/.local/etc/bash_completion.d ]; then
    for f in ~/.local/etc/bash_completion.d/*; do
        . $f
    done
fi

export EDITOR=$EMACS_TERM_CLIENT
export VISUAL=$EMACS_TERM_CLIENT
export BROWSER='chromium'

source /orcam/env/scripts/rcfile # this also sets the prompt, so you have to override it after

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

if [ "$PS1" ]; then
    # If this is an xterm set the title to user@host:dir
    case $TERM in
        xterm*|rxvt*)
            PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
            ;;
        *)
            ;;
    esac
fi


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
if [ "$PS1" ]; then
    # don't put duplicate lines in the history. See bash(1) for more options
    export HISTCONTROL=ignoredups
fi



# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Set usefule variables for the env
export tcdir=$HOME/local/toolchains
export brdir=$HOME/local/buildroots

function maybe_add_ssh_key(){
    if ! ssh-add -l > /dev/null; then
        echo line1
        ssh-add $1
    elif ! ssh-add -l | grep -q `ssh-keygen -lf $1  | awk '{print $2}'`; then
        echo line2
        ssh-add $1
    fi
}

# source ~/.local/bin/ssh-find-agent.sh
# set_ssh_agent_socket
if ! pgrep ssh-agent > /dev/null; then
    eval `ssh-agent` && echo ssh-agent started successfully
else
    echo ssh-agent already running
fi

# (pgrep ssh-agent > /dev/null && [[ $? == 1 ]] && eval `ssh-agent` && \
#      echo ssh-agent started successfully ) || \
#     ( pgrep ssh-agent > /dev/null && [[ $? == 0 ]] && echo ssh-agent already running ) || \
#     echo ssh-agent failed to start
maybe_add_ssh_key /orcam/env/scripts/baseunit_ssh_key/id_rsa
maybe_add_ssh_key ~/.ssh/id_bitbucket_rsa

