(defun toggle-kbd-macro-recording-on ()
    "One-key keyboard macros: turn recording on."
      (interactive)
        (define-key
              global-map
                  (this-command-keys)
                      'toggle-kbd-macro-recording-off)
          (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
    "One-key keyboard macros: turn recording off."
      (interactive)
        (define-key
              global-map
                  (this-command-keys)
                      'toggle-kbd-macro-recording-on)
          (end-kbd-macro))

(global-set-key (kbd "C-q") 'toggle-kbd-macro-recording-on)
(provide 'single-key-kbd)
