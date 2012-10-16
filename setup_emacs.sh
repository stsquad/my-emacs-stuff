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

EMACS_CONFIG_HOME="$HOME/.emacs.d"

#
# Create the ./emacs.d
#
if [ ! -d "${EMACS_CONFIG_HOME}" ] ; then
  echo "Creating .emacs.d"
  mkdir ${EMACS_CONFIG_HOME}
fi

#
# Copy in the *.el files
#
for file in *.el
do
  if [ ! -f ${EMACS_CONFIG_HOME}/$file ] ; then
    echo "Linking $file to .emacs.d"
    linkfile=`pwd`/$file
    target=${EMACS_CONFIG_HOME}/$file
    echo "Linking $linkfile to $target"
    ln -s $linkfile $target
  fi
done

#
# Link to the snippets hierarchy
#
if [ ! -L ${EMACS_CONFIG_HOME}/my-snippets ]; then
    echo "Linking in snippets"
    linkdir=`pwd`/snippets
    ln -s $linkdir "${EMACS_CONFIG_HOME}/my-snippets"
fi
