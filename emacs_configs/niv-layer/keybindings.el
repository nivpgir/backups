;;; Keybinds:

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'niv-layer/smarter-move-beginning-of-line)
;;remap C-; to expand-region
(global-set-key (kbd "C-;") 'er/expand-region)


(global-set-key (kbd  "C-/"  ) nil)

(define-key undo-tree-map (kbd "C-/")  nil)

(global-set-key  (kbd  "C-/"  ) 'comment-line)
