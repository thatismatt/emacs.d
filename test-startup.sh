#!/bin/sh -e

# see https://github.com/purcell/emacs.d/blob/master/test-startup.sh

cd $(readlink -e $(dirname $0))

echo "Attempting startup..."
emacs -nw --batch \
      --eval '(let ((debug-on-error t)
                    (url-show-status nil)
                    (user-emacs-directory default-directory)
                    (user-init-file (expand-file-name "init.el"))
                    (load-path (delq default-directory load-path)))
                 (load-file user-init-file)
                 (run-hooks (quote after-init-hook)))'
echo "Startup successful"
