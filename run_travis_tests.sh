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

# Now we have started we can install our normal package set
if [ "$EMACS" = 'emacs23' ] ; then
    # For Emacs23 we need to manually install ELPA
    ${EMACS} -q --batch -l ./tests/install-elpa.el -f my-install-elpa
fi

# Install all the packages we use
${EMACS} -q --batch -l ./my-package.el -f my-packages-reset

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

# Now we are set-up we can the ERT tests (if we are running Emacs24 or above)
if [ "$EMACS" = 'emacs24' ] ; then
    ${EMACS} -q --batch -l "tests/my-ert.el" -f ert-run-tests-batch-and-exit
fi
