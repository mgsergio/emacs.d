(use-package helm
  :config
  (require 'helm-config)
  (setq helm-buffes-fuzzy-matching t)
  (helm-mode t))


(use-package helm-swoop)


(use-package helm-projectile
  :config
  (helm-projectile-on))
