#!/bin/bash
#
# Check the Daemon starts up
#
set -ex
echo -en "travis_fold:start:daemon_starts.$1\r"
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
echo -en "travis_fold:end:daemon_starts.$1\r"
exit 0
