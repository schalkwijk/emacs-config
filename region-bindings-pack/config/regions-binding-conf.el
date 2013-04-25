;;integration with multiple-cursors

;; multiple cursors stuff
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "e" 'mc/edit-lines)
(define-key region-bindings-mode-map "E" 'mc/edit-ends-of-lines)
(define-key region-bindings-mode-map "A" 'mc/edit-beginnings-of-lines)

;;random
(define-key region-bindings-mode-map ";" 'comment-dwim)
(define-key region-bindings-mode-map (kbd "TAB") 'indent-region)
(define-key region-bindings-mode-map (kbd "R") 'align-regexp)


(defun surround-region-with-delim (open-delim &optional close-delim)
  (if (region-active-p)
      (save-excursion
        (goto-char (region-beginning))
        (insert open-delim)
        (goto-char (region-end))
        (insert (or close-delim open-delim)))))


;; surround region
(mapc
 (lambda (delim-pair)
   (let ((surround-region-form `(surround-region-with-delim ,(car delim-pair) ,(car (cdr delim-pair)))))
     (define-key region-bindings-mode-map (car delim-pair) `(lambda () (interactive) (eval ,surround-region-form)))
     (if (cdr delim-pair) (define-key region-bindings-mode-map (car (cdr delim-pair)) `(lambda () (interactive) (eval ,surround-region-form))))))
 `(("(" ")") ("\"") ("[" "]") ("|")))
