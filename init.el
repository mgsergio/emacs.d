(require 'package)
(package-initialize) ; force all packages to load

;; My Emacs settings
(add-to-list 'load-path "~/.emacs.d/init.d")

;; Custom styles
(add-to-list 'load-path "~/.emacs.d/styles")

;; Add repos with lots of cool packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(require 'look-and-feel)
(require 'emacs-behaviour)

;; Toggle cua-mode
(cua-mode)
(setq cua-enable-cua-keys nil)  ;; Disable cua c-x/c-c/c-v bindings


;; Dear speedbar, please show me all files,
;; not just those you know
(custom-set-variables
 '(speedbar-show-unknown-files t))

(defun my-sr-speedbar-toggle-and-select ()
  (interactive)
  (sr-speedbar-toggle)
  (sr-speedbar-select-window))

;; Toggle speedbar with F12
(global-set-key (kbd "<f12>") 'my-sr-speedbar-toggle-and-select)


;; Nice item list instead of buffer of choices
(require 'ido)   ; TODO Use ehlm instead
(ido-mode t)
(setq ido-enable-flex-match t)

;; TODO: Move to behaviour
(move-text-default-bindings)
(fullscreen-mode)

;; Setup cider, a repl mode for clojure, and more
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (setq nrepl-hide-special-buffers t) ;; hiding special buffers
;; (etq cider-repl-tab-command 'indent-for-tab-command) ;; tab ke behaviour in repl


;; cc-mode Settings
(add-to-list 'auto-mode-alist
	     '("\\.h\\'" . c++-mode))  ; C++-mode for .h files

(require 'metrika-c++-style)
(add-hook 'c-initialization-hook 'metrika-set-c++-style)

;; Useful key bindings
(global-set-key (kbd "<f5>") 'compile)


;; TODO: Use helm-projectile
