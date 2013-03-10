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

;; create a temporarly snippet file that can be used to write YARD-docs
(defun insert-list (list)
  (while list
    (insert (concat "\n" (first list)))
    (setq list (cdr list))))

(defun create-YARD-doc-snippet ()
  (interactive)
  (save-excursion
    (re-search-forward "def [^(]+")
    (if (looking-at "(\\\(.*\\\))")
        (let ((value (match-string 0)))
          (with-temp-buffer
            (insert "# name: docstring\n# key: doc\n#-*- require-final-newline: nil -*-\n# --\n# $1")
            (let ((index 1))
              (insert-list (mapcar (lambda (param) (incf index) (concat "# @param " (format "[$%d] %s $%d" index param (incf index))))
                                   (mapcar (lambda (arg) (nth 0 (split-string arg "="))) (split-string (replace-regexp-in-string "[()[[:blank:]]]*" "" value) ","))))
              (incf index)
              (insert (concat "\n# @return " (format "[$%d]" index) " $0")))
            (write-region (point-min) (point-max) "~/.emacs.d/etc/snippets/ruby-mode/yard/doc")))))
  (live-reload-snippets)
  (insert "doc")
  (yas/expand))

;; occur-mode buff
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; in dired, open up all files that have been marked
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'dired-open-all-marked-files)
     (defun dired-open-all-marked-files (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))


;; set ruby mode for gemfiles
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

;; auto-revert dired buffers
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
