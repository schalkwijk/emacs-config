(defun copy-line-by-num (line &optional buffer)
  "Copy LINE, counting from line 1 at beginning of buffer.
If called interactively, a numeric prefix argument specifies
LINE; without a numeric prefix argument, read LINE from the
minibuffer.

If optional argument BUFFER is non-nil, switch to that buffer and
copy line LINE there.  If called interactively with \\[universal-argument]
as argument, BUFFER is the most recently selected other buffer.
"
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     ;; Look for a default, a number in the buffer at point.
     (let* ((buffer ;; Decide if we're switching buffers.
             (if (consp current-prefix-arg)
                 (other-buffer (current-buffer) t)))
            (buffer-prompt
             (if buffer
                 (concat " in " (buffer-name buffer))
               "")))
       ;; Read the argument, offering that number (if any) as default.
       (list (read-number (format "Copy line%s: " buffer-prompt))
             buffer))))
  ;; Switch to the desired buffer, one way or another.
  (if buffer
      (let ((window (get-buffer-window buffer)))
        (if window (select-window window)
          (switch-to-buffer-other-window buffer))))

  ;; Move to the specified line number in that buffer, copy it, and bail
  (save-excursion                       ; don't know if there is a better way of doing this
    (move-beginning-of-line nil)
    (insert (save-excursion
      (save-restriction
         (widen)
         (goto-char (point-min))         ; go to beggining fo buffer
         (forward-line (1- line))        ; jump to desired line
         (let ((beg (point)))
           (forward-line)
           (filter-buffer-substring beg (point))))))))
