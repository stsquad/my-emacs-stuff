#!/bin/sh
#
# Check the Daemon starts up
#
set -ex
emacs --version

emacsclient -a '' -e "(setq my-auto-shutdown-daemon-flag t)"
sleep 30
pgrep emacs
cat ~/emacs-startup.txt

exit 0
