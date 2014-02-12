#!/bin/bash
#
# Run the tests under a Travis VM.
#
set -ex

# For the logs
${EMACS} --version

# Dump our env
#env

# Setup the Emacs environment
./setup_emacs.sh

# Basic start-up in daemon test
${EMACS} --daemon
OK=`emacsclient -e "(if I-completed-loading-dotinit 0 -1)"`
if [ "$OK" != "0" ]
then
    echo "Failed --daemon start-up with clean package list"
    exit -1
fi
emacsclient -e "(kill-emacs)"

# Install all the packages we use
${EMACS} -q --batch ${EXTRA_LOAD} -l ./my-package.el -f my-packages-reset

# Check the state of .emacs.d
echo "Checking installed packages"
find ~/.emacs.d -iname "*.el"
ls -l ~/.emacs.d

# Restart the daemon
${EMACS} --daemon
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
${EMACS} -q --batch -l "tests/compile-setup.el" -f batch-byte-compile ~/.emacs.d/*.el
${EMACS} -q --batch -l "tests/compile-setup.el" -f batch-byte-compile ~/.emacs.d/my-elisp/*.el

# Now we are set-up we can the ERT tests (if we are running Emacs24 or above)
if [ "$EMACS" = 'emacs24' ] ; then
    ${EMACS} -q --batch -l "tests/my-ert.el" -f ert-run-tests-batch-and-exit
fi
