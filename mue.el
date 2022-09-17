(setf inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell nil
      make-backup-files nil
      indent-tabs-mode nil)

(setf help-window-select t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)

(set-fringe-mode 15)

(load-theme 'adwaita t)

(fido-vertical-mode t)
