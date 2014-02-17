#!/bin/bash
#
# Check the Daemon starts up
#
set -ex
echo "Installing ELPA/MELPA packages"
emacs --version
emacs -q --batch ${EXTRA_LOAD} -l ./my-package.el -f my-packages-reset

# Check the state of .emacs.d
echo "Checking installed packages"
find ~/.emacs.d -iname "*.el"
ls -l ~/.emacs.d
exit 0
