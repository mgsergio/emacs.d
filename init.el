(setf inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell nil
      make-backup-files nil)

(set-default 'indent-tabs-mode nil)

(setf help-window-select t)

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-fringe-mode 15)))
(menu-bar-mode -1)
(global-hl-line-mode 1)


(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; TODO: Expand to all directories with site-listp/.
(add-to-list 'load-path
             "/opt/homebrew/share/emacs/site-lisp/maxima" t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(unbind-key "C-z" global-map)

;; TODO: (desktop-save-mode 1)
;; TODO: here and in mue.el trim trailing whitespaes!

;; Wanted packages/features:
;; Rename both file and buffer
;; helm/swiper
;; multiple-cursors
;; mode-line?
;; window-management
;; hydra?
;; grow/shrink regions?
;; tree-sitter
;; ace-jump ace-* ?
;; smartparent
;; expand-region
;; copany-mode

(require 'use-package-ensure)
(setf use-package-always-ensure t)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package typescript-mode
  :mode "\\.tsx?\\'")

(use-package python-isort
  :hook (python-mode . python-isort-on-save-mode))
(use-package blacken
  :hook (python-mode . blacken-mode))
;; (add-hook python-mode-hook #'flycheck-mode))

(use-package which-key
  :config (which-key-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  :commands (smex smex-initialize))

;; TODO: Also setup helm and try it later.
(use-package counsel
  :config (ivy-mode 1)
  :bind (("C-s" . 'swiper-isearch)
         ("M-x" . 'counsel-M-x)
         ("M-y". 'counsel-yank-pop)
         ("C-x C-f" . 'counsel-find-file)
         ("C-x b" . 'ivy-switch-buffer)
         ("C-c f j" . 'counsel-file-jump)))


(defvar custom-keymap (make-sparse-keymap))
(require 'bind-key) ;; bind-key is a part of use-package.

(bind-keys :map custom-keymap
           ("s a" . counsel-ag)
           ("s r" . counsel-rg)
           ("t w" . writeroom-mode)
           ("t W" . whitespace-mode)
           ("t c" . flycheck-mode)
           ("t z" . zoom-mode)
           ("M-r" . ivy-resume)
           ("s-r" . ivy-resume))

(bind-key "s-p" custom-keymap)
(bind-key "M-p" custom-keymap)

;; --------
;; Search:
;;  <prefix> j g . git grep
;;  <prefix> j r . rg
;;  <prefix> j a . ag
;;  counsel-ack
;;  occur
;;  multi-occur
;;
;; --------
;; Toggle:
;;  writeroom
;;  flycheck
;;  zoom
;;  evil
;;  whitespace
;;  lsp/eglot?
;;  TreeMacs
;;
;; --------
;; Moving
;;  paragraphs
;;  s-exp
;;  functions
;;
;; --------
;; Jumps
;;  definition
;;  search for a symbol by name
;;  bookmark?
;;
;; --------
;; C-` Jump to terminal and back to the buffer (create term if not exists)
;;
;; --------
;; etc:
;;  ivy-resume
;;
;; --------
;; lsp bindings



(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package zoom
  :config (zoom-mode 1))
(use-package writeroom-mode
  :init (global-writeroom-mode)
  :config
  (setf writeroom-width 120
        writeroom-mode-line t
        ;; writeroom-header-line t
        ;; writeroom-maximize-window nil
        writeroom-global-effects (remq 'writeroom-set-fullscreen
                                       writeroom-global-effects)))


;; TODO: (use-package buffer-move)


(use-package vc-fossil
  ;; Keep from loading unnecessarily at startup.
  :defer t
  ;; This allows VC to load vc-fossil when needed.
  :init (add-to-list 'vc-handled-backends 'Fossil t))

(use-package magit
  :config
  (with-eval-after-load 'project
    ;; Make [m] magit available when switching projects.
    (require 'magit-extras)))


;; TODO: try (use-package eglot)
;; TODO: lsp-mode overrides flycheck checkers by default so no next checker is set
;;       after ls. Wich is kinda annoying for python for instance.
(use-package lsp-mode
  :init
  (setf lsp-keymap-prefix "C-c l")
  :hook (((typescript-mode js-mode) . lsp-deferred)
         ;; (python-mode . lsp-deferred)
         ;; which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))
(use-package lsp-ui)
(use-package lsp-ivy)

(use-package solarized-theme)
;; (use-package nord-theme)
(add-hook 'after-init-hook
          (lambda ()
            (load-theme (if (display-graphic-p)
                            'solarized-selenized-dark
                          'solarized-gruvbox-dark))))


(use-package evil
  :init
  (setf evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
