#!/usr/bin/env bash
# every directory should have a conf_setup.sh script inside it that links
# all the relevant config files to the correct places, all this script
# needs to do is iterate over all the directories that need to be setup
# (each directory that requires setup should be past as an argument)
# and run the conf_setup.sh script inside it.
# optional addition:
# maybe pass some default variables to the conf_setup script for it to
# use (buildroot style) to have more control over what gets backed up, I
# didn't do it right now because I can't think of something that the scripts
# are lacking, but this idea might be usefule in the future.
for d in $@; do
    pushd $d
    ./conf_setup.sh
    popd
done

