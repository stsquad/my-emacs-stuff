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

# Now we have started we can install our normal package set
if [ "$EMACS" = 'emacs23' ] ; then
    # For Emacs23 we need to manually install ELPA
    emacsclient -e "(kill-emacs)"
    ${EMACS} -q --batch -l "tests/install-elpa.el" -e "(my-install-elpa)"
    ${EMACS} --daemon
fi

# Install the new package set
INSTALL=`emacsclient -e "(my-packages-reset)"`
echo "Install: $INSTALL"
sleep 30
# dump the current list
INSTALLED=`emacsclient -e "package-activated-list"`
sleep 30

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
    ${EMACS} -q --batch -l "~/.emacs.d/init.el" -f batch-byte-compile ~/.emacs.d/*.el
else
    ${EMACS} -q --batch -l "~/.emacs.d/init.el" -l ert -l "tests/my-ert.el" -f ert-run-tests-batch-and-exit
fi
