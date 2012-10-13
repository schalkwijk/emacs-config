;;integration with multiple-cursors

;;when region is defined
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)

;; From active region to multiple cursors:
(define-key region-bindings-mode-map "e" 'mc/edit-lines)
(define-key region-bindings-mode-map "E" 'mc/edit-ends-of-lines)
(define-key region-bindings-mode-map "A" 'mc/edit-beginnings-of-lines)

;;random
(define-key region-bindings-mode-map ";" 'comment-dwim)
