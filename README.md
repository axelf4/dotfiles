# dotfiles

## Installation

```sh
git clone --bare https://github.com/axelf4/dotfiles.git $HOME/.dotfiles
alias cfg="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
cfg checkout
cfg config --local status.showUntrackedFiles no
```

### Install [straight.el]

```sh
git clone https://github.com/radian-software/straight.el.git ~/.config/emacs/straight/repos/straight.el
```

## Bash startup files

The table below shows what files Bash executes on startup.

<table>
	<caption>Bash startup files</caption>
	<tr>
		<td></td>
		<th scope="col">Interactive</th>
		<th scope="col">Non-interactive</th>
	</tr>
	<tr>
		<th scope="row">Login</th>
		<td colspan="2"><code>/etc/profile</code> and then first readable of<br><code>~/.bash_profile</code>, <code>~/.bash_login</code>, <code>~/.profile</code></td>
	</tr>
	<tr>
		<th scope="row">Non-login</th>
		<td><code>/etc/bash.bashrc</code> and <code>~/.bashrc</code></td>
		<td><code>$BASH_ENV</code></td>
	</tr>
</table>

To have have login shells too read the shell configuration file
(e.g. because tmux always starts login shells),
create a `.bash_profile` with
```bash
[[ -f ~/.profile ]] && . ~/.profile
[[ -f ~/.bashrc ]] && . ~/.bashrc
```
Note however that it is then necessary to test if interactive (with `$-`)
before trying to change terminal settings.

[straight.el]: https://github.com/raxod502/straight.el
