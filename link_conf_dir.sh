#!/bin/bash

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

# the working directory should have the same name as the diretory
dir_local=$HOME/.config/$(basename $(pwd))
dir_repo=$(realpath .)

# conky configuring is easy, all we need is to backup the original
backup_if_exists $dir_local
# and link to this directory
ln -s $dir_repo $dir_local
