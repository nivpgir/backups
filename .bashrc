#
# ~/.bashrc
#

# If not running interactively, don't do anything



[[ $- != *i* ]] && return

#envars for programs requiring to set an editor
EDITOR=emacs
VISUAL=emacs

alias ls='ls -lh --color=auto'
alias em='emacs'
alias mv='mv -i'
alias rm='rm -i'



reset=$(tput sgr0)
black=$(tput setaf 0)
red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
purple=$(tput setaf 5)
teal=$(tput setaf 6)
white=$(tput setaf 7)

source ~/Gits/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
GITPROMPT=$(__git_ps1 " (%s)")

PS1='\[$blue\]\u \[$yellow\]\w\[$purple\]$(__git_ps1 " (%s)")\[$yellow\] $>\[$reset\] '

