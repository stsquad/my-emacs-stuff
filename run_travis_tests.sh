#!/bin/bash
#
# Run the tests under a Travis VM.
#
set -ex

# For the logs
${EMACS} --version

# Dump our env
env

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

# Now we have started we can install our normal package set
INSTALL=`emacsclient -e "(my-packages-reset)"`
echo "Install: $INSTALL"

# seems a little drastic but it works
emacsclient -e "(kill-emacs)"
sleep 10

${EMACS} --daemon
OK=`emacsclient -e "(if I-completed-loading-dotinit 0 -1)"`
if [ "$OK" != "0" ]
then
    echo "Failed --daemon start-up with normal package list"
    exit -1
fi

# Now we are set-up we can the ERT tests (if we are running Emacs24 or above)
if [ "$EMACS" = 'emacs23' ] ; then
    echo "Skipping ERT for Emacs23"
else
    ${EMACS} -q --batch -l "~/.emacs.d/init.el" -l ert -l "tests/my-ert.el" -f ert-run-tests-batch-and-exit
fi
