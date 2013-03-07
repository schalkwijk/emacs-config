(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (simp-project-root) "/TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

;; build ctags for current project - note that this
;; requires the simp pack
(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (concat (simp-project-root) "/")))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=log --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags))

(defun etags-select-find-tag-at-point-wrapped ()
  (interactive)
  (if (file-exists-p (concat (simp-project-root) "/TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))

(global-set-key (kbd "M-.")  'etags-select-find-tag-at-point-wrapped)
(global-set-key (kbd "\M-?") 'etags-select-find-tag)
