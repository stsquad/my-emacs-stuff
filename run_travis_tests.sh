#!/bin/bash
#
# Run the tests under a Travis VM.
#
set -ex

# For the logs
${EMACS} --version

# Setup the Emacs environment
./setup_emacs.sh
emacs --daemon


