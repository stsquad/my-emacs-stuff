#!/bin/bash
#
# Run the tests under a Travis VM.
#
set -ex

emacs --version
# Dump our env
#env

# Setup the Emacs environment
./setup_emacs.sh

# Basic start-up in daemon test
emacs --daemon
OK=`emacsclient -e "(if I-completed-loading-dotinit 0 -1)"`
if [ "$OK" != "0" ]
then
    echo "Failed --daemon start-up with clean package list"
    exit -1
fi
emacsclient -e "(kill-emacs)"

# Install all the packages we use
emacs -q --batch ${EXTRA_LOAD} -l ./my-package.el -f my-packages-reset

# Check the state of .emacs.d
echo "Checking installed packages"
find ~/.emacs.d -iname "*.el"
ls -l ~/.emacs.d

# Restart the daemon
emacs --daemon
OK=`emacsclient -e "(if I-completed-loading-dotinit 0 -1)"`
if [ "$OK" != "0" ]
then
    echo "Failed --daemon start-up with normal package list"
    exit -1
else
    INSTALLED=`emacsclient -e "package-activated-list"`
    echo "Fully loaded --daemon with pkg=${INSTALLED}"
fi
emacsclient -e "(kill-emacs)"

#
# Now check we can compile everything
EMACS_COMPILE="emacs -q --batch -l tests/compile-setup.el -f batch-byte-compile"
${EMACS_COMPILE} ~/.emacs.d/init.el
find ~/.emacs.d/my-elisp/ -maxdepth 1 -iname "my-*.el" | grep -v my-email | xargs -n 1 ${EMACS_COMPILE}

# Now we are set-up we can the ERT tests (if we are running Emacs24 or above)
if [ "$EMACS" = 'emacs24' ] ; then
    emacs -q --batch -l "tests/my-ert.el" -f ert-run-tests-batch-and-exit
fi
