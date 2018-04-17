#!/bin/bash

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ls='ls --color=auto'
alias ll='ls -lF --color=auto'
alias lal='ls -al --color=auto'
alias la='ls -a --color=auto'
alias l='ls -CF --color=auto'

# prettier pushd stack printing
alias d='dirs -v'

alias mv='mv -i'

EMACS_CLIENT="emacsclient -q -c -n -a ''"
EMACS_TERM_CLIENT="emacsclient -q -c -t -a ''"
alias em="$EMACS_TERM_CLIENT -t $@"
alias emacs="$EMACS_CLIENT $@"

# Add an "alert" alias for long running commands.  Use like so:
# e.g  sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# git aliases
alias gc="git commit"
alias gs="git status"
alias gd="git diff"
alias gunadd="git reset HEAD "
alias gco="git checkout"
alias gcob="git 'checkout -b"
alias gclone="git clone --recursive"
alias gps="git push"
alias gpl="git pull"
alias gfe="git fetch"
alias ga="git add"

alias diff="diff --color"
