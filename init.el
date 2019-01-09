;; TODO:
;; hydra
;; ace-jump
;; ace-window
;; and better windo management in general
;; neo-tree or alike and better
;; perspectives/worspaces
;; discover or similar
;; which-key
;; python + completion and staff
;; docker
;; shell env (load .bashrc in emacs shell mode)
;;
;; Maybe switch to ivi instead of helm? ...
;;
;; Check this out.
;; https://dev.to/huytd/emacs-from-scratch-1cg6

(package-initialize)
(add-to-list 'package-archives
	     ;; many packages won't show if using stable
	     ;; '("melpa" . "http://stable.melpa.org/packages/")
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)

;; keep the installed packages in .emacs.d
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))
(when (not package-archive-contents)
  (package-refresh-contents))

(package-install 'use-package)

;; TODO: Make it rebember last buffer for each window.
(defun srj/last-opened-buffer ()
  "Cycles between cvurrent buffer and the prevous visited one"
  (interactive)
  (switch-to-buffer
   (other-buffer (current-buffer) t)
   nil t))

(use-package general
  :ensure t)

(use-package evil
  :ensure t
  :config (evil-mode t))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode t))

;; Unset space in motion mode to avoid conflict.
(general-def evil-motion-state-map "SPC" nil)
(general-def 'motion Info-mode-map "SPC" nil)
(general-def Info-mode-map "SPC" nil)

(general-def
 :states '(normal motion)
 :prefix "SPC"
 ;; "ff" 'find-file
 "fs" 'save-buffer
 ;; "ss" 'swiper
 ;; TODO: Should be in visual as well.
 ;; "SPC" 'counsel-M-x
 "TAB" 'srj/last-opened-buffer
 ;; "bb" 'counsel-ibuffer
 "bd" 'kill-this-buffer
 "bD" 'kill-buffer
 "bR" 'revert-buffer

 "hdv" 'describe-variable
 "hdf" 'describe-function
 "hdv" 'describe-variable
 "hdk" 'describe-key
 "hi" 'info
 )


;; Indent with tab in normal mode.
;; TODO: Is there a better way? I.e. Override motion definition with normal one.
(general-def :states 'motion "TAB" nil)
(general-def :states 'normal "TAB" 'indent-for-tab-command)

(use-package projectile
  :ensure t
  :general
  (:prefix "SPC"
   :states '(normal motion)
   :keymaps 'projectile-mode-map
   "p" projectile-command-map)
  :config (projectile-mode t))

(use-package helm
  :ensure t
  :general
  (:prefix "SPC"
   :states '(normal motion visual)
   "SPC" 'helm-M-x
   "ff" 'helm-find-files
   "bb" 'helm-mini
   )
  :config (helm-mode t))

(use-package helm-swoop
  :ensure t
  :general
  (:prefix "SPC"
   :states '(normal motion visual)
   "ss" 'helm-swoop))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package magit
  :ensure t
  :general
  (:prefix "SPC"
   :states '(normal motion)
   "gs" 'magit-status))

(use-package smart-comment
  :ensure t
  :general
  (:prefix "SPC"
   :states '(normal motion visual)
   "; ;" 'smart-comment))

;; TODO:
;; (use-package which-key
;;   :ensure t
;;   :config (which-key-mode t))

(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))

(use-package centered-window
  :ensure t
  :general
  (:prefix "SPC"
   :states '(normal motion)
   "wc" 'centered-window-mode)
  :config
  (setq cwm-left-fringe-ratio 50)
  (centered-window-mode t))

;; Languages
(use-package go-mode
  :commands go-mode
  :ensure t)


;; Themes
(package-install 'monokai-theme)
(load-theme 'monokai t)

;; Some defaults.
(setq
 ;; delete excess backup versions silently
 delete-old-versions t
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))

 version-control t
 vc-follow-symlinks t
 vc-make-backup-files t

 ;; use utf-8 by default
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8

 ;; sentence SHOULD end with only a point.
 sentence-end-double-space nil

 ;; toggle wrapping text at the 80th character
 default-fill-column 80

 ;; print a default message in the empty scratch buffer opened at startup
 initial-scratch-message "Welcome in Emacs"
 ;; silent bell when you make a mistake
 ring-bell-function 'ignore
 inhibit-startup-screen t
 )

;; Disable tool-bar
(tool-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Customs.
;; TODO: Move this shit out of init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-projectile helm evil-escape monokai-theme monokai-them smex which-key magit projectile evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
