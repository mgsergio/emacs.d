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
 initial-scratch-message "Welcome to Emacs"
 ;; silent bell when you make a mistake
 ring-bell-function 'ignore
 inhibit-startup-screen t

 custom-file "~/.emacs.d/custom.el"
 )

;; Disable tool-bar
(tool-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(load custom-file)

(add-hook 'before-save-hook 'whitespace-cleanup)

(show-paren-mode 1)
