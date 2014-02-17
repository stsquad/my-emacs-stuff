#!/bin/bash
#
# Check the init files compile
#
set -ex
echo "Checking init files compile"
emacs --version
for file in ~/.emacs.d/my-elisp/*.el
do
    if [ ! $file == "my-email.el" ]; then
        echo "Checking $file compiles"
        emacs -q --batch -l tests/compile-setup.el -f batch-byte-compile $file
    fi
done
exit 0
