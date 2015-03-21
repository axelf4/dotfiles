#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

git pull origin master;

rsync --exclude ".git/" --exclude "bootstrap.sh" --exclude "README.md" -avh --no-perms . ~;