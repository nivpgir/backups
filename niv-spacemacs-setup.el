(provide 'niv-spacemacs-setup)

;;; Functions:
;smarter 'goto-beggining-of-line':
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


; Configuration:
;; idk:
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; globally center on the cursor
(global-centered-cursor-mode)
;;enable company-flx for fuzzy completion in complay-idle-completion
;;(with-eval-after-load 'company
;;  (company-flx-mode +1))

;;company fuzzy search
(setq company-search-regexp-function (lambda (search-term)
                                       (mapconcat 'identity (seq-map 'string search-term) ".*")
                                       )
      )
;; to remove currnetly selected region on type without having to manually delete
(delete-selection-mode)


; Keybinds:
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
;;remap C-; to expand-region
(global-set-key (kbd "C-;") 'er/expand-region)
