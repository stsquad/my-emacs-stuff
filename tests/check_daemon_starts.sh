#!/bin/sh
#
# Check the Daemon starts up
#
set -ex
emacs --version
emacs --daemon

sleep 10

ps ax | grep emacs

CHECK_STATUS=$(emacsclient -e '(if I-completed-loading-dotinit 0 -1)')
echo "got $CHECK_STATUS"

if test "$CHECK_STATUS" = "0"
then
    echo "Failed --daemon start-up"
    exit 1
else
    INSTALLED=$(emacsclient -e "package-activated-list")
    echo "Succesful --daemon start-up with pkg=${INSTALLED}"
fi
emacsclient -e "(kill-emacs)"
exit 0
