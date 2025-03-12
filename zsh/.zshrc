[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

source ~/.zprezto/init.zsh
source ~/.profile

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

stty -ixon

if [[ -z $EMACS ]]; then
    export EDITOR='vim'
else
    export EDITOR='emacsclient -n'
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=6'
fi

compinit -u
source ~/.machine_zshrc

# Golang setup
export GOPATH=$HOME/gopackages
export PATH=$PATH:$GOPATH/bin:~/.local/bin
export PATH=$PATH:$HOME/.npm-global/bin

function revert-expand-or-complete {
  zle expand-or-complete
}

zle -N expand-or-complete-with-indicator revert-expand-or-complete

# stack config set system-ghc --global true

[ -f "/usr/local/google/home/mandhane/.ghcup/env" ] && source "/usr/local/google/home/mandhane/.ghcup/env" # ghcup-env
