# =============================================================================
#                                   Functions
# =============================================================================
powerlevel9k_random_color(){
	local code
	#for code ({000..255}) echo -n "$%F{$code}"
	#code=$[${RANDOM}%11+10]    # random between 10-20
	code=$[${RANDOM}%211+20]    # random between 20-230
	printf "%03d" $code
}

zsh_wifi_signal(){
	local signal=$(nmcli -t device wifi | grep '^*' | awk -F':' '{print $6}')
    local color="yellow"
    [[ $signal -gt 75 ]] && color="green"
    [[ $signal -lt 50 ]] && color="red"
    echo -n "%F{$color}\uf1eb" # \uf1eb is 
}

# =============================================================================
#                                   Variables
# =============================================================================
# If not using zplug, you need to source this:
[[ -f $POWERLEVEL9K_LOCATION ]] && source $POWERLEVEL9K_LOCATION

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Configuracion POWERLVEL9K
# POWERLEVEL9K_MODE='awesome-patched'
POWERLEVEL9K_MODE='nerdfont-complete'

POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"

# Elementos de la barra
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir dir_writable vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time background_jobs status time ssh)
POWERLEVEL9K_STATUS_VERBOSE=true

POWERLEVEL9K_DIR_HOME_BACKGROUND='09'
POWERLEVEL9K_DIR_DEFAULT_BACKGROUND='09'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND='009'
POWERLEVEL9K_DIR_HOME_FOREGROUND='236'
POWERLEVEL9K_DIR_DEFAULT_FOREGROUND='236'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND='236'

# `git hub colors`
POWERLEVEL9K_VCS_CLEAN_BACKGROUND='236'
POWERLEVEL9K_VCS_CLEAN_BACKGROUND='119'
POWERLEVEL9K_VCS_CLEAN_FOREGROUND='236'
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='214'
POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND='236'
POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='196'
POWERLEVEL9K_VCS_MODIFIED_FOREGROUND='236'

# Quitar iconos del inicio
POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_HOME_SUB_ICON=''
POWERLEVEL9K_FOLDER_ICON=''


# =============================================================================
#                                   Plugins
# =============================================================================
# Check if zplug is installed
[ ! -d ~/.zplug ] && git clone https://github.com/zplug/zplug ~/.zplug
# #source ~/.zplug/init.zsh && zplug update
source ~/.zplug/init.zsh

# #zplug "b4b4r07/enhancd", use:init.sh
# zplug "b4b4r07/enhancd", use:enhancd.sh
# #zplug "b4b4r07/zsh-vimode-visual", defer:3
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme
# #zplug "knu/zsh-manydots-magic", use:manydots-magic, defer:2
zplug "seebi/dircolors-solarized", ignore:"*", as:plugin

# #zplug "plugins/bundler", from:oh-my-zsh, if:"which bundle"
zplug "plugins/colored-man-pages", from:oh-my-zsh
# #zplug "plugins/extract", from:oh-my-zsh
# #zplug "plugins/fancy-ctrl-z", from:oh-my-zsh
# #zplug "plugins/globalias", from:oh-my-zsh
zplug "plugins/gpg-agent", from:oh-my-zsh, if:"which gpg-agent"
# #zplug "plugins/httpie", from:oh-my-zsh, if:"which httpie"
# #zplug "plugins/nanoc", from:oh-my-zsh, if:"which nanoc"
# #zplug "plugins/vi-mode", from:oh-my-zsh

zplug "plugins/git",    from:oh-my-zsh, if:"which git"
# zplug "plugins/go",     from:oh-my-zsh, if:"which go"
# zplug "plugins/golang", from:oh-my-zsh, if:"which go"
# zplug "plugins/nmap",   from:oh-my-zsh, if:"which nmap"
zplug "plugins/sudo",   from:oh-my-zsh, if:"which sudo"
# zplug "plugins/tmux",   from:oh-my-zsh, if:"which tmux"

# # Supports oh-my-zsh plugins and the like
# if [[ $OSTYPE = (linux)* ]]; then
# 	zplug "plugins/archlinux", from:oh-my-zsh, if:"which pacman"
# 	zplug "plugins/dnf",       from:oh-my-zsh, if:"which dnf"
# fi

# if [[ $OSTYPE = (darwin)* ]]; then
# 	zplug "plugins/osx",      from:oh-my-zsh
# 	zplug "plugins/brew",     from:oh-my-zsh, if:"which brew"
# 	zplug "plugins/macports", from:oh-my-zsh, if:"which port"
# fi

zplug "zsh-users/zsh-completions",              defer:0
zplug "zsh-users/zsh-autosuggestions",          defer:2, on:"zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting",      defer:3, on:"zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search", defer:3, on:"zsh-users/zsh-syntax-highlighting"

# =============================================================================
#                                   Options
# =============================================================================

# improved less option
export LESS="--tabs=4 --no-init --LONG-PROMPT --ignore-case --quit-if-one-screen --RAW-CONTROL-CHARS"

# Watching other users
#WATCHFMT="%n %a %l from %m at %t."
watch=(notme)         # Report login/logout events for everybody except ourself.
#watch=(all)         # Report login/logout events for everybody except ourself.
LOGCHECK=60           # Time (seconds) between checks for login/logout activity.
REPORTTIME=5          # Display usage statistics for commands running > 5 sec.
#WORDCHARS="\"*?_-.[]~=/&;!#$%^(){}<>\""
WORDCHARS="\"*?_-[]~&;!#$%^(){}<>\""

# History
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

setopt append_history           # Dont overwrite history
setopt extended_history         # Also record time and duration of commands.
setopt share_history            # Share history between multiple shells
setopt hist_expire_dups_first   # Clear duplicates when trimming internal hist.
setopt hist_find_no_dups        # Dont display duplicates during searches.
setopt hist_ignore_dups         # Ignore consecutive duplicates.
setopt hist_ignore_all_dups     # Remember only one unique copy of the command.
setopt hist_reduce_blanks       # Remove superfluous blanks.
setopt hist_save_no_dups        # Omit older commands in favor of newer ones.

# Changing directories
#setopt auto_pushd
setopt pushd_ignore_dups        # Dont push copies of the same dir on stack.
setopt pushd_minus              # Reference stack entries with "-".

setopt extended_glob

# =============================================================================
#                                   Aliases
# =============================================================================

# In the definitions below, you will see use of function definitions instead of
# aliases for some cases. We use this method to avoid expansion of the alias in
# combination with the globalias plugin.

[[ -f ~/.aliases ]] && source ~/.aliases

# Directory coloring
alias ls='() { $(whence -p ls) -Ctr --file-type --color=auto $@ }'

# Generic command adaptations.
alias grep='() { $(whence -p grep) --color=auto $@ }'
alias egrep='() { $(whence -p egrep) --color=auto $@ }'

# =============================================================================
#                                Key Bindings
# =============================================================================

# # Common CTRL bindings.
# bindkey "^a" beginning-of-line
# bindkey "^e" end-of-line
# bindkey "^f" forward-word
# bindkey "^b" backward-word
# bindkey "^k" kill-line
# bindkey "^d" delete-char
# bindkey "^y" accept-and-hold
# bindkey "^w" backward-kill-word
# bindkey "^u" backward-kill-line
# bindkey "^R" history-incremental-pattern-search-backward
# bindkey "^F" history-incremental-pattern-search-forward

# # Do not require a space when attempting to tab-complete.
# bindkey "^i" expand-or-complete-prefix

# # Fixes for alt-backspace and arrows keys
# bindkey '^[^?' backward-kill-word
# bindkey "^[[1;5C" forward-word
# bindkey "^[[1;5D" backward-word

# =============================================================================
#                                 Completions
# =============================================================================

zstyle ':completion:*' rehash true
#zstyle ':completion:*' verbose yes
#zstyle ':completion:*:descriptions' format '%B%d%b'
#zstyle ':completion:*:messages' format '%d'
#zstyle ':completion:*:warnings' format 'No matches for: %d'
#zstyle ':completion:*' group-name ''

# case-insensitive (all), partial-word and then substring completion
zstyle ":completion:*" matcher-list \
  "m:{a-zA-Z}={A-Za-z}" \
  "r:|[._-]=* r:|=*" \
  "l:|=* r:|=*"

zstyle ":completion:*:default" list-colors ${(s.:.)LS_COLORS}

# =============================================================================
#                                   Startup
# =============================================================================

# # Load SSH and GPG agents via keychain.
# setup_agents() {
#   [[ $UID -eq 0 ]] && return

#   if which keychain &> /dev/null; then
# 	local -a ssh_keys gpg_keys
# 	for i in ~/.ssh/**/*pub; do test -f "$i(.N:r)" && ssh_keys+=("$i(.N:r)"); done
# 	gpg_keys=$(gpg -K --with-colons 2>/dev/null | awk -F : '$1 == "sec" { print $5 }')
#     if (( $#ssh_keys > 0 )) || (( $#gpg_keys > 0 )); then
# 	  alias run_agents='() { $(whence -p keychain) --quiet --eval --inherit any-once --agents ssh,gpg $ssh_keys ${(f)gpg_keys} }'
# 	  [[ -t ${fd:-0} || -p /dev/stdin ]] && eval `run_agents`
# 	  unalias run_agents
#     fi
#   fi
# }

# # setup_agents
# # unfunction setup_agents

# Install plugins if there are plugins that have not been installed
if ! zplug check; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load

if zplug check "seebi/dircolors-solarized"; then
  if which gdircolors &> /dev/null; then
	alias dircolors='() { $(whence -p gdircolors) }'
  fi
  if which dircolors &> /dev/null; then
    scheme="dircolors.256dark"
    eval $(dircolors ~/.zplug/repos/seebi/dircolors-solarized/$scheme)
  fi
fi

# # History
# if zplug check "zsh-users/zsh-history-substring-search"; then
# 	zmodload zsh/terminfo
# 	bindkey "$terminfo[kcuu1]" history-substring-search-up
# 	bindkey "$terminfo[kcud1]" history-substring-search-down
# 	bindkey "^[[1;5A" history-substring-search-up
# 	bindkey "^[[1;5B" history-substring-search-down
# fi

# [ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"

# Source local customizations.
POWERLEVEL9K_LOCATION=/usr/share/zsh-theme-powerlevel9k/powerlevel9k.zsh-theme

# other sourcing
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
[[ -f ~/.zshrc.alias ]] && source ~/.zshrc.alias
[[ -f ~/.zfunctions ]] && source ~/.zfunctions
