#!/usr/bin/env sh

CACHE_DIR="cache"

mkdir -p $CACHE_DIR > /dev/null 2>&1

EMACS_LISP="
  (let ((args '(:file         \"simple-test.org\"
                :cache-dir    \"$CACHE_DIR\"
                :output-file  \"simple-test.html\"
                :htmlize      \"true\"
                :theme        \"tango\"
                :user-config  \"\"
                )))
      (hexo-render-org args))
"

EMACS_LISP=${EMACS_LISP//$'\n'/} # Remove all newlines.
EMACS_LISP=${EMACS_LISP%$'\n'}   # Remove a trailing newline.

emacs -Q -nw -l init.el --eval "${EMACS_LISP}"
