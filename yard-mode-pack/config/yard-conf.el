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
  (yas-expand))
