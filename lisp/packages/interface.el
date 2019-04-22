(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))

(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))

;; TODO(mgserjio): Maybe writeroom is better.
(use-package centered-window
  :config
  (setq cwm-left-fringe-ratio 50)
  (centered-window-mode t))
