;; hydra
;; general?
;; ace-jump
;; ace-window
;; neo-tree or alike and better
;; perspectives/worspaces
;; discover.or similar

(package-initialize)
(add-to-list 'package-archives
	     ;; many packages won't show if using stable
	     ;; '("melpa" . "http://stable.melpa.org/packages/")
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)

;; keep the installed packages in .emacs.d
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))
(package-refresh-contents)

(package-install 'use-package)

;; evil-magit
(use-package evil
  :ensure t
  :config (evil-mode t))

(use-package projectile
  :ensure t
  :config (projectile-mode t))

(use-package ivy
  :ensure t
  :config (ivy-mode t))

(use-package counsel
  :ensure t
  :config (counsel-mode t))

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package smex
  :ensure t)

;; Themes
(package-install 'monokai-theme)
(load-theme 'monokai t)

;; Some defaults.
;; TODO: Check them out.
(setq delete-old-versions -1)		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files t)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t)				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;transform backups file name
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup

;; Disable tool-bar
(tool-bar-mode -1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme monokai-them smex which-key magit projectile evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
