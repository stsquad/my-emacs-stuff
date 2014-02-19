#!/bin/bash
#
# Check the Daemon starts up
#
set -ex
echo -en 'travis_fold:start:install_pkgs\r'
emacs --version
emacs -q --batch ${EXTRA_LOAD} -l ./my-package.el -f my-packages-reset

# Check the state of .emacs.d
echo "Checking installed packages"
find ~/.emacs.d -iname "*.el"
ls -l ~/.emacs.d
echo -en 'travis_fold:end:install_pkgs\r'
exit 0
