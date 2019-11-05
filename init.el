;; TODO: Fix style.

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)


;; (add-to-list 'default-frame-alist '(font . "mononoki-12"))
;; (add-to-list 'default-frame-alist '(height . 24))
;; (add-to-list 'default-frame-alist '(width . 80))

;; Show matching parens
(setf show-paren-delay 0)
(show-paren-mode 1)

;; Disable stupid beeping.
(setf ring-bell-function 'ignore)

;; Disable backup files
(setf make-backup-files nil  ; stop creating backup~ files
      auto-save-default nil) ; stop creating #autosave# files

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Package configs
(require 'package)
(setf package-enable-at-startup nil)
(setf package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootsrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; ;; Vim mode
;; (use-package evil
;;   :ensure t
;;   :init
;;   ;; Note: From https://github.com/emacs-evil/evil-collection:
;;   ;;       evil-collection assumes evil-want-keybinding is set to nil and evil-want-integration
;;   ;;       is set to t before loading evil and evil-collection. Note some other packages may
;;   ;;       load evil (e.g. evil-leader) so bear that in mind when determining when to set the
;;   ;;       variables.
;;   ;;
;;   ;;       See https://github.com/emacs-evil/evil-collection/issues/60
;;   ;;       and https://github.com/emacs-evil/evil/pull/1087 for more details.
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-escape
;; 	     :ensure t
;; 	     :init
;; 	     ;; TODO: Investigate or make a PR to rebind or disable
;; 	     ;;       evil-escape in sertain evil states.
;; 	     (setq-default evil-escape-key-sequence "fd"
;; 			   evil-escape-delay 0.1)
;; 	     :config 
;; 	     (evil-escape-mode 1))

;; (use-package evil-collection
;;   :ensure t
;;   :init
;;   (evil-collection-init))
  

;; Helm
(use-package helm
  :ensure t
  :init
  (setf helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t

        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (helm-mode 1))


;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))


(use-package magit
  :ensure t)


;; Which Key
(use-package which-key
  :ensure t
  :init
  (setf which-key-separator " "
        which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


;; ;; Custom keybinding
;; (use-package general
;;   :ensure t
;;   :config (general-define-key
;;   :states '(normal visual insert emacs)
;;   :prefix "SPC"
;;   :non-normal-prefix "M-SPC"
;;   ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
;;   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
;;   "SPC" '(helm-M-x :which-key "M-x")
;;   "ff"  '(helm-find-files :which-key "find files")
;;  
;;   ;; Buffers
;;   "bb"  '(helm-buffers-list :which-key "buffers list")
;;   "bk"  '(kill-buffer :which-key "Kill buffer")
;;  
;;   ;; Window
;;   "wl"  '(windmove-right :which-key "move right")
;;   "wh"  '(windmove-left :which-key "move left")
;;   "wk"  '(windmove-up :which-key "move up")
;;   "wj"  '(windmove-down :which-key "move bottom")
;;   "w/"  '(split-window-right :which-key "split right")
;;   "w-"  '(split-window-below :which-key "split bottom")
;;   "wx"  '(delete-window :which-key "delete window")
;;  
;;   ;; Others
;;   ;; "at"  '(ansi-term :which-key "open terminal")
;; ))


;; TODO:
;;
;; Fix problem with environment variables.
;; (let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path 
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))
