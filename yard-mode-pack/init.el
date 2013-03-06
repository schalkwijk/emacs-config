;; YARD syntax highlighting for ruby
;; http://rubydoc.info/docs/yard/

(live-add-pack-lib "yard-mode")
(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
