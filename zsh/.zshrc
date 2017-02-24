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
fi

compinit -u
source ~/.machine_zshrc
