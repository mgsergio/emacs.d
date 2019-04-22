;; Check this out.
;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; And this
;; https://www.youtube.com/watch?v=6INMXmsCCC8

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


(add-to-list 'load-path "~/.emacs.d/lisp")


(defun user/load-file (file-name)
  (load-file (expand-file-name file-name "~/.emacs.d/lisp/user")))

(defun user/load-package (package-name)
  (load-file (expand-file-name package-name "~/.emacs.d/lisp/packages")))


(user/load-file "settings.el")
(user/load-file "themes.el")


(user/load-package "use-package.el")

:; TODO: helm, ivi, ido, etc belong to interaction.el
(user/load-package "helm.el")
(user/load-package "editor.el")
(user/load-package "interface.el")
(user/load-package "languages.el")
(user/load-package "projectile.el")

(add-to-list 'load-path "~/.emacs.d/lisp/tom")
(require 'tom-mode)


;; TODO: Make it rebember last buffer for each window.
(defun srj/last-opened-buffer ()
  "Cycles between cvurrent buffer and the prevous visited one"
  (interactive)
  (switch-to-buffer
   (other-buffer (current-buffer) t)
   nil t))

;; Prevent python repl from echoing input.
;; (add-hook 'inferior-python-mode-hook (lambda () (setq comint-process-echoes t)))
