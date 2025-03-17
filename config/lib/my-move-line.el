(defun move-line (direction)
  (let ((col (current-column)))
    (unless (eq col 0)
      (move-to-column 0))
    (save-excursion
      (forward-line)
      (transpose-lines direction))
    (forward-line direction)))

(defun move-line-down ()
  (interactive)
  (move-line 1))

(defun move-line-up ()
  (interactive)
  (move-line -1))

(provide 'my-move-line)
