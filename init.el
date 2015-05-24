(require 'package)
(setq package-list '(ascii-art-to-unicode
		     automargin cider
		     clojure-mode ess
		     fullscreen-mode
		     helm-projectile
		     helm async jabber
		     monokai-theme
		     move-text
		     projectile
		     pkg-info
		     epl f dash s))

;; My Emacs settings
(add-to-list 'load-path "~/.emacs.d/init.d")

;; Custom styles
(add-to-list 'load-path "~/.emacs.d/styles")

;; Add repos with lots of cool packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize) ; force all packages to load

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install mising packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Load setting from .org files in .emacs.d/org
(require 'ob-tangle)
(let ((org-files-directory (expand-file-name "org" user-emacs-directory)))
  (mapc #'org-babel-load-file
	(directory-files org-files-directory t "\\.org$")))
