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

    # echo "this: $full_layer_path will point to this: $(realpath ${l})"
    ln -s $(realpath ${l}) ${full_layer_path}
done

# finally, backup and link .spacemacs:
dot_spacemacs=.spacemacs
prefix_basename $HOME/$dot_spacemacs link_dot_spacemacs
real_dot_spacemacs=$(realpath ${dot_spacemacs})
# echo "this: $link_dot_spacemacs will point to this: $real_dot_spacemacs"
backup_if_exists ${real_dot_spacemacs}
ln -s $(realpath ${real_dot_spacemacs}) ${link_dot_spacemacs}
