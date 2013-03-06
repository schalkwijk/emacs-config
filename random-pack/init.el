;;Just some arbitrary bindings I didn't want to pollute ~/.emacs.d/init.el with

;; open magit status
(global-set-key (kbd "C-x g s") 'magit-status)

;; some global key definitions
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key (kbd "C-<tab>") 'indent-region)
(define-key global-map "\C-xj" 'dired-jump)

;; quick switching to previous buffer
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x y") 'switch-to-previous-buffer)

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

;; random custom commands
(load-file (concat (live-pack-lib-dir) "/random.el"))

;; full screen git status

;; (defadvice git-status (around git-fullscreen activate)
;;   (window-configuration-to-register :git-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))

;; (defun git-quit-session ()
;;   "Restores the previous window configuration and kills the git buffer"
;;   (interactive)
;;   (kill-buffer)
;  (jump-to-register :git-fullscreen))

;(define-key git-status-mode-map (kbd "q") 'git-quit-session)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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
