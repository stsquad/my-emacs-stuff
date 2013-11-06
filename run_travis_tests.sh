#!/bin/bash
#
# Run the tests under a Travis VM.
#
set -ex

# For the logs
${EMACS} --version

# Setup the Emacs environment
./setup_emacs.sh

# Basic start-up in daemon test
${EMACS} --daemon
OK=`emacsclient -e "(if I-completed-loading-dotinit 0 -1)"`
if [ "$OK" != "0" ]
then
    echo "failed start-up"
    exit -1
fi

# TODO: add some ERT tests?
