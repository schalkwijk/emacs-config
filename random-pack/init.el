;;Just some arbitrary bindings I didn't want to pollute ~/.emacs.d/init.el with

;; some global key definitions
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key (kbd "C-<tab>") 'indent-region)
(define-key global-map "\C-xj" 'dired-jump)

;;quick switching to previous buffer
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x y") 'switch-to-previous-buffer)

;;delete whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;correct javascript indentation
(setq js-indent-level 2)

;;make dired not create a new buffer when using ^ to navigate
(add-hook 'dired-mode-hook
          (lambda ()
          (define-key dired-mode-map (kbd "^")
            (lambda () (interactive) (find-alternate-file "..")))))

;;zap-up-to-char
 (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.
  \(fn arg char)"
    'interactive)
(global-set-key "\M-Z" 'zap-up-to-char)

 ; Outline-minor-mode key map
 (define-prefix-command 'cm-map nil "Outline-")
 ; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
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
(global-set-key "\M-o" cm-map)
