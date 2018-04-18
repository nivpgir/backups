#!/bin/sh
revert() {
    xset dpms 0 0 0
}
trap revert HUP INT TERM
xset +dpms dpms 5 5 5
if [ $# != 0 ]; then
    echo "using image $1"
    i3lock -n -i $1
else
    i3lock -n
fi
revert
