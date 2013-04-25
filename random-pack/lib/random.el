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


;; set ruby mode for gemfiles, rakefiles
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '(".rake" . ruby-mode))

;; get occur do do some more intuitive navigation
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

(defun occur-dwim ()
  (interactive)
  (let ((search-for (if (region-active-p)
                        (buffer-substring (region-beginning) (region-end))
                      (thing-at-point 'symbol))))
    (if search-for
        (occur search-for current-prefix-arg)
      (message "Failed to occur. No active region, and point not near a symbol"))))

(define-key global-map (kbd "M-s O") 'occur-dwim)

;; some more global key definitions
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key (kbd "C-<tab>") 'indent-region)
(define-key global-map "\C-xj" 'dired-jump)

;; delete whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; correct javascript indentation
(setq js-indent-level 2)

;; make dired not create a new buffer when using ^ to navigate
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

;; zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  \(fn arg char)"
  'interactive)
(global-set-key "\M-Z" 'zap-up-to-char)

;; copy, instead of killing, a line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key "\C-c\C-k" 'copy-line)

                                        ; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")

(define-key cm-map "q" 'hide-sublevels) ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
                                        ; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
                                        ; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key (kbd "C-M-o") cm-map)


;; have ido hop over to symbol
(global-set-key (kbd "C-'") 'live-ido-goto-symbol)

;; revert all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; add C-M-backspace to kill segxp backwards
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; dired diff marked files
(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))

  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files))
            (buffer-file-name (nth 2 marked-files)))))

;; windmove
(windmove-default-keybindings)         ; shifted arrow keys
(setq windmove-wrap-around t)

;; With this snippet, another press of C-d will kill the buffer.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; With these shortcuts you can open a new line above or below the current one, even if the cursor is midsentence.
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;; For some reason, renaming the current buffer file is a multi-step process in Emacs.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(global-set-key (kbd "C-x r o") 'bookmark-jump-other-window)

(global-set-key (kbd "C-x M-b") 'ido-switch-buffer-other-window)
