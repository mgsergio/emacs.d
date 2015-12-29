(require 'package)
(setq package-list '(ascii-art-to-unicode
		     centered-window-mode
		     cider
		     clojure-mode ess
		     fullscreen-mode
		     helm-projectile
		     helm async jabber
		     monokai-theme
		     auto-complete
		     move-text
		     projectile
		     pkg-info
		     multiple-cursors
		     magit
		     epl f dash s))

;; Misc emacs scripts/funcitons
(add-to-list 'load-path "~/.emacs.d/etc")

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

;; Start emacs server unless we have one
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'server)
	    (unless (server-running-p)
	      (server-start))))

;; Play with semantic
(setf semanticdb-project-roots '("/Users/mgsergio/omim"))
(setf semantic-default-submodes '(global-semanticdb-minor-mode
				  global-semantic-idle-scheduler-mode
				  global-semantic-stickyfunc-mode))
(semantic-mode 1)


;; Autogenerated staff here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   (quote
    (("Programming"
      ("Organizer"
       (used-mode . org-mode))
      ("Project"
       (filename . ".*.pro"))
      ("Sources"
       (used-mode . c++-mode))))))
 '(ibuffer-saved-filters
   (quote
    (("gnus"
      ((or
	(mode . message-mode)
	(mode . mail-mode)
	(mode . gnus-group-mode)
	(mode . gnus-summary-mode)
	(mode . gnus-article-mode))))
     ("programming"
      ((or
	(mode . emacs-lisp-mode)
	(mode . cperl-mode)
	(mode . c-mode)
	(mode . java-mode)
	(mode . idl-mode)
	(mode . lisp-mode)))))))
 '(org-agenda-files nil)
 '(speedbar-show-unknown-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#272822")))))
