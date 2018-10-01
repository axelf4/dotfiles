# dotfiles

## Installation

```
git clone --bare https://github.com/axelf4/dotfiles.git $HOME/.dotfiles
alias cfg="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
cfg checkout
cfg config --local status.showUntrackedFiles no
```

### Install vim-plug

```
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```
