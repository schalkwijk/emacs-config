;; random stuff
(key-chord-define-global "hj" 'undo)    ;fast undo
(key-chord-define-global ";;" 'comment-dwim) ;insert comment

;;jump-to-char stuff
(key-chord-define-global "df" 'jump-char-forward)
(key-chord-define-global "fg" 'jump-char-backward)

;; zap-to-char magic
(key-chord-define-global "zz" 'zap-up-to-char)

;;ace-jump keys
(key-chord-define-global "jk" 'ace-jump-word-mode)

;; yasnippets
(key-chord-define-global "fj" 'yas/expand)
