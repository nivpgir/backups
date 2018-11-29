;;; Functions:

;;smarter 'goto-beggining-of-line':
(defun niv-layer/smarter-move-beginning-of-line (arg)
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


(defun niv-layer/c-copy-proto-to-header-file ()
  (interactive)
  (save-excursion
    ;; c-mode's `beginning-of-defun' should be robust enough.
    (beginning-of-defun)
    (let ((l (point)))
      (search-forward-regexp " *{")
      (let ((proto (buffer-substring l (match-beginning 0))))
        (ff-find-other-file)
        ;; If other file is already open, we don't want to move point.
        (save-excursion
          (goto-char (point-max))
          ;; Do some more movement here if you want.
          (insert "\n" proto ";"))))))
