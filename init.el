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

(require 'use-package-ensure)
(setf use-package-always-ensure t)

(setf debug-on-error t)

(org-babel-load-file "~/.emacs.d/config.org")

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
