#!/bin/bash


source ../utils.sh
emacs_conf_dir=$(realpath $HOME/.emacs.d)
emacs_prv_dir=$emacs_conf_dir/private
layers="niv-layer"
for l in $layers; do
    full_layer_path=${emacs_prv_dir}/${l}

    # same general concept with other conf dir, but for each layer
    # backup the layer if it exists, then link it to the layer in here
    # this is easier bacause we know no layer is a hidden dir
    backup_if_exists ${full_layer_path}
    # echo $(realpath ${l})
    # echo ${full_layer_path}
    ln -s $(realpath ${l}) ${full_layer_path}
done

# finally, backup and link .spacemacs and .spacemacs.env:
# dot_spacemacs=.spacemacs
# prefix_basename $dot_spacemacs target_spacemacs
# echo $target_spacemacs
# real_dot_spacemacs=$(realpath ${HOME}/${dot_spacemacs})
# echo $real_dot_spacemacs
# backup_if_exists ${real_dot_spacemacs}
# ln -s $(realpath ${target_spacemacs}) ${real_dot_spacemacs}

# dot_spacemacs_env=.spacemacs.env
# prefix_basename $dot_spacemacs_env target_spacemacs_env
# echo $target_spacemacs_env
# real_dot_spacemacs_env=$(realpath ${HOME}/${dot_spacemacs_env})
# echo $real_dot_spacemacs_env
# backup_if_exists ${real_dot_spacemacs_env}
# ln -s $(realpath ${target_spacemacs_env}) ${real_dot_spacemacs_env}

function restore_prefixed_file(){
    backup_file=$1
    restore_dir=$2
    prefix_basename $backup_file prefixed_backup_file
    backup_if_exists $restore_dir/$backup_file
    ln -s $prefixed_backup_file $restore_dir/$backup_file
}

restore_prefixed_file .spacemacs $HOME
restore_prefixed_file .spacemacs.env $HOME
