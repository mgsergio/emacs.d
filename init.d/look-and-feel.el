;; Disable useless controls
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show column and line numbers
(column-number-mode)
;;(global-linum-mode)

;; Set a cool sublime theme
(load-theme 'monokai t)

;; Disable stupid beeping
(setq ring-bell-function 'ignore)

(provide 'look-and-feel)
