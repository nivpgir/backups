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



# load local bash completions
if [ -d ~/.local/etc/bash_completion.d ]; then
    for f in ~/.local/etc/bash_completion.d/*; do
        . $f
    done
fi

export EDITOR=em
export VISUAL=emacs
export BROWSER='chromium'


function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

function venv_name {
		local venvname=${VIRTUAL_ENV##*/}
		if [[ -z "$venvname" ]]; then
				echo ""
		else
				echo "($venvname)"
		fi
}

set_prompt(){
    Last_Command=$? # Must come first!
    Blue='\[\e[01;34m\]'
    White='\[\e[01;37m\]'
    BoldRed='\[\e[01;31m\]'
    Red='\[\e[00;31m\]'
    Green='\[\e[01;32m\]'
		Yellow='\[\e[1;32m\]'
		Orange='\[\e[38;5;202m\]'
		Swamp='\[\e[38;5;58m\]'
    LightSwamp='\[\e[38;5;106m\]'
		Orcam1='\[\e[01;36m\]'
    Orcam2='\[\e[01;33m\]'
    Reset='\[\e[0m\]'
    FancyX='\342\234\227'
    Checkmark='\342\234\223'
    NoColor='\[\e[0m\]'
		OrangeBG='\[\e[48;5;202m\]'
		SwampBG='\[\e[48;5;58m\]'
		git_branch_bg=$Orange
		py_venv_bg=$LightSwamp


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

    # export PS1="${PS1}${White}"'[\t] [\[\033[0;31m\]\u@\h\[\033[00m\]] [\[\033[1;36m\]\w\[\033[00m\]]$(parse_git_branch)$(venv_name)\n\[\033[1;32m\]$ \[\033[00m\]'

		export PS1="${PS1}${White}""[\t] [$Red\u@\h$Reset] [$Orcam1\w$Reset]$git_branch_bg$(parse_git_branch)$Reset$py_venv_bg$(venv_name)$Reset\n$Yellow$ $Reset"

}
export PROMPT_COMMAND="set_prompt"


# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000000
HISTFILESIZE=2000000
HISTFILE=/home/$USER/.histfile
export HISTCONTROL=ignoredups
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
export WORKON_HOME=~/.py_venvs
[[ -f ~/.local/bin/virtualenvwrapper.sh ]] && source ~/.local/bin/virtualenvwrapper.sh

[[ -f $HOME/.local_bashrc ]] && . $HOME/.local_bashrc



