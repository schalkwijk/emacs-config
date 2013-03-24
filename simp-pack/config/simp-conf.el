(simp-project-define
   '(:type ruby
     :has (Gemfile)
     :ignore-directories (.git log doc)
     :ignore-files (.rvmrc .rspec TAGS)))

(simp-project-define
   '(:type emacs
     :has (init.el)
     :ignore-directories (.git)
     :ignore-files (TAGS)))

(global-set-key (kbd "C-c f") 'simp-project-find-file)
(global-set-key (kbd "C-c d") 'simp-project-root-dired)
(global-set-key (kbd "C-c s") 'simp-project-rgrep)
(global-set-key (kbd "C-c S") 'simp-project-rgrep-dwim)
(global-set-key (kbd "C-c b") 'simp-project-ibuffer-files-only)
(global-set-key (kbd "C-c B") 'simp-project-ibuffer)

;; The functions below no longer seem to work
;;(global-set-key (kbd "C-c C-f") 'simp-project-with-bookmark-find-file)
;;(global-set-key (kbd "C-c C-b") 'simp-project-with-bookmark-ibuffer)
;;(global-set-key (kbd "C-c C-s") 'simp-project-with-bookmark-rgrep)
