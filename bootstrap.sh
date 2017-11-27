#!/usr/bin/env bash
#
# bootstrap installs things.

set -e
DOTFILES_ROOT=$(pwd)

info () {
	printf "  [ \033[00;34m..\033[0m ] $1"
}

user () {
	printf "\r  [ \033[0;33m?\033[0m ] $1 "
}

success () {
	printf "\r\033[2K  [ \033[00;32mOK\033[0m ] $1\n"
}

fail () {
	printf "\r\033[2K  [\033[0;31mFAIL\033[0m] $1\n"
	echo ''
	exit
}

link_file () {
	local src=$1 dst=$2

	local overwrite= backup= skip=
	local action=

	if [ -f "$dst" -o -d "$dst" -o -L "$dst" ]
	then

		if [ "$overwrite_all" == "false" ] && [ "$backup_all" == "false" ] && [ "$skip_all" == "false" ]
		then

			local currentSrc="$(readlink $dst)"

			if [ "$currentSrc" == "$src" ]
			then
				skip=true;
			else
				user "File already exists: $dst ($(basename "$src")), what to do?\n[s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all?"
				read -n 1 action

				case "$action" in
					o ) overwrite=true;;
					O ) overwrite_all=true;;
					b ) backup=true;;
					B ) backup_all=true;;
					s ) skip=true;;
					S ) skip_all=true;;
					* ) ;;
				esac
			fi
		fi

		overwrite=${overwrite:-$overwrite_all}
		backup=${backup:-$backup_all}
		skip=${skip:-$skip_all}

		if [ "$overwrite" == "true" ]
		then
			rm -rf "$dst"
			success "Removed $dst"
		fi

		if [ "$backup" == "true" ]
		then
			mv "$dst" "${dst}.backup"
			success "Moved $dst to ${dst}.backup"
		fi

		if [ "$skip" == "true" ]
		then
			success "Skipped $src"
		fi
	fi

	if [ "$skip" != "true" ]  # "false" or empty
	then
		mkdir -p "$(dirname "${dst}")"
		ln -s "$1" "$2"
		success "Linked $1 to $2"
	fi
}

install_dotfiles () {
	info 'Installing dotfiles'

	local overwrite_all=false backup_all=false skip_all=false

	for src in $(find -H "$DOTFILES_ROOT" -name '.*')
	do
		# dst="$HOME/$(basename "${src%.*}")"
		dst="$HOME/$(basename $src)"
		if [ -d "$src" ]
		then
			mkdir -p "$dst"
		else
			link_file "$src" "$dst"
		fi
	done
}

git pull origin master
install_dotfiles

while true; do
	read -p "Install vim-plug? " yn
	case $yn in
		[Yy]* ) curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim; break;;
		[Nn]* ) break;;
		* ) echo "Please answer yes or no.";;
	esac
done

echo ''
echo '  All installed!'
