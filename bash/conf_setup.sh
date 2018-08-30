#!/bin/bash

# linking bash configs
backup_if_exists(){
    # if the file itself exists, and there is no backup file,
    # create a backup file and exit
    if [ -e $1 ]; then
        if [ ! -e $1.bak ]; then
            local backup_file=$1.bak
            echo backing up $1 to $backup_file
        else
            # else, while filename.bak.$i exists, increment i, then create filename.bak.$i
            i=1
            while [ -e $1.bak.$i ]; do
                echo $1.bak.$i exists...
                ((i++))
            done
            local backup_file=$1.bak.$i
            echo backing up $1 to $backup_file
        fi
        mv $1 $backup_file
    fi
    # if file doesn't exist, we have nothing to do
}

prefix_basename(){
    # if it's a hidden file, we need to prefix "dot" to it,
    # cause that's how I keep dotfiles in the repo
    local base=`basename $1`
    # local dir=`dirname $1`
    local prefixed=${base/./dot.}
    if [ ! -z "$2" ]; then
        # this creates a variable named as the expanded value of $2
        # which it's value will be the expanded value of $prefixed
        eval $2="'$prefixed'"
    else
        echo $prefixed
    fi
}

backup_orig_and_link_new(){
    backup_if_exists $2
    ln -s $1 $2
}

# conf_files="$HOME/.bashrc $HOME/.bash_aliases"
conf_files="$HOME/.bashrc $HOME/.bash_aliases"
# each file is given as a full path to where the link needs to be
for f in $conf_files; do
    # if the file exists we backupto make room for the link
    backup_if_exists $f
    # then we prefix with "dot" cause we don't keep the files hidden in the repo
    # and also take only the basename because the files should be here locally
    # and we'll take the realpath to them
    prefix_basename $f target
    ln -s $(realpath $target) $(realpath $f)
done
