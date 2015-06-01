######## COMPLETION ###########################################################

autoload -U compinit && compinit
autoload -U colors && colors
zstyle ':completion:*:default' list-colors $LS_COLORS
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

# case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

######## OTHER CUSTOMIZATION ##################################################

# juicy correction
setopt correct # but not too juicy

# vim style line editing
bindkey -v

# prompt
export PROMPT=$'%F{white}%~ %B%#%b '

# window title (assuming term is urxvt)

# Write some info to terminal title.
# This is seen when the shell prompts for input.
function precmd {
	print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
}

# Write command and args to terminal title.
# This is seen while the shell waits for a command to complete.
function preexec {
	printf "\033]0;%s\a" "$1"
}

######## HISTORY ##############################################################

export HISTSIZE=2000
export SAVEHIST=2000
export HISTFILE=~/.zhistory

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

######## ALIASES ##############################################################

# color
alias ls='ls --color --sort=extension'
alias grep='grep --color'
alias tree='tree -C'
alias pacman='pacman --color=always'

# customizing
alias info='info --vi-keys'
alias big='xrandr --output VBOX0 --mode 1440x900;'
alias small='xrandr --output VBOX0 --mode 1440x836;'
alias scheme='racket -il xrepl'

# to clear the screen without scrollback
alias cls="echo -ne '\033c'"

# because typing is hard
alias n="urxvt -cd \$(pwd) &"

# magic to make sudo aliases work (eg make 'sudo pacman' color)
alias sudo='sudo '

# the systemd hype isn't really real, but we can pretend
alias sctl='systemctl'
alias uctl='systemctl --user'
alias jctl='journalctl'

# use neovim instead of vim
alias vi='nvim'
alias vim='nvim'

######## FUNCTIONS ############################################################

# make tools
source ~/.zsh/build.zsh
