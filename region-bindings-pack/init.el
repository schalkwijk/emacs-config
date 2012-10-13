;;region bindings mode -- allows you to have shortcut keys when the region is active
(require 'region-bindings-mode)
(live-load-config-file "regions-binding-conf.el")
(region-bindings-mode-enable)
