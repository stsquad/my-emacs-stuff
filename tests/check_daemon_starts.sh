#!/bin/bash
#
# Check the Daemon starts up
#
set -ex
echo "Checking --daemon starts"
emacs --version
emacs --daemon
OK=`emacsclient -e "(if I-completed-loading-dotinit 0 -1)"`
if [ "$OK" != "0" ]
then
    echo "Failed --daemon start-up"
    exit -1
else
    INSTALLED=`emacsclient -e "package-activated-list"`
    echo "Succesful --daemon start-up with pkg=${INSTALLED}"
fi
emacsclient -e "(kill-emacs)"
exit 0
