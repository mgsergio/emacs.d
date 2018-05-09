(require 'package)
(setf package-selected-packages '(helm
                                  helm-projectile
                                  monokai-theme
                                  solarized-theme
                                  auto-complete
                                  move-text
                                  multiple-cursors
                                  magit
                                  fullscreen-mode
                                  writeroom-mode
                                  haskell-mode
                                  js2-mode
                                  yaml-mode
                                  cmake-mode
                                  slime
                                  cider
                                  ace-window
                                  ace-jump-mode
                                  exec-path-from-shell
                                  neotree
                                  restclient
                                  restclient-helm
                                  clang-format
                                  vlf
                                  ess
                                  rg
                                  zoom))

;; Misc emacs scripts/funcitons
(add-to-list 'load-path "~/.emacs.d/etc")

;; Custom styles
(add-to-list 'load-path "~/.emacs.d/styles")

;; Add repos with lots of cool packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize) ; force all packages to load

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install mising packages
(package-install-selected-packages)

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

;; Store customs outside init.el
(setf custom-file "~/.emacs.d/custom-set-variables.el")
(load custom-file :NOERROR)
