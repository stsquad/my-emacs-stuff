#!/bin/sh
#
# Check the Daemon starts up
#
set -ex
emacs --version
emacs --daemon

sleep 1

if test $(emacsclient -e "(if I-completed-loading-dotinit 0 -1)") -eq 0
then
    echo "Failed --daemon start-up"
    exit 1
else
    INSTALLED=$(emacsclient -e "package-activated-list")
    echo "Succesful --daemon start-up with pkg=${INSTALLED}"
fi
emacsclient -e "(kill-emacs)"
exit 0
