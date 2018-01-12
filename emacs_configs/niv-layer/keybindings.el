;;; Keybinds:

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'niv-layer/smarter-move-beginning-of-line)
;;remap C-; to expand-region
(global-set-key (kbd "C-;") 'er/expand-region)

;; set C-c C-d to duplicate line
(global-set-key (kbd "C-c d") 'spacemacs/duplicate-line-or-region)

;; set C-/ to 'undo'
(global-set-key (kbd  "C-/") nil)

(define-key undo-tree-map (kbd "C-/")  nil)

(global-set-key  (kbd  "C-/") 'comment-line)
