#!/bin/bash
#
# Alex's Emacs setup script.
#
# Install the .emacs init script in my current home directory
# using symbolic links. Also create a ~/.emacs.d directory
# and copy in the .el files that are managed in this repo
#
# This script assumes its in the repo and all dotfiles need to go to $HOME
#

for file in dot*
do
  dotfile=`echo $file | sed s/dot/\./`
  if [ -f $HOME/$dotfile ] ; then
    echo "backing up $HOME/$dotfile"
    mv $HOME/$dotfile $HOME/$dotfile.orig
  fi
  linkfile=`pwd`/$file
  echo "Linking $linkfile to ~/$dotfile"
  ln -s $linkfile ~/$dotfile
done

#
# Create the ./emacs.d
#
if [ ! -d "$HOME/.emacs.d" ] ; then
  echo "Creating .emacs.d"
  mkdir $HOME/.emacs.d
fi

#
# Copy in the *.el files
#
for file in *.el
do
  if [ ! -f $HOME/.emacs.d/$file ] ; then
    echo "Linking $file to .emacs.d"
    linkfile=`pwd`/$file
    target=$HOME/.emacs.d/$file
    echo "Linking $linkfile to $target"
    ln -s $linkfile $target
  fi
done