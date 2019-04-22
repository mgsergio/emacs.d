(use-package go-mode
  :commands go-mode)

(use-package yaml-mode
  :commands yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (general-def yaml-mode-map "\C-m" 'newline-and-indent))

(use-package terraform-mode
  :commands terraform-mode)
