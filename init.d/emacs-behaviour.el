;; Tune tabs alignment
(setq tab-stop-list (number-sequence 4 200 4))

;; Highlight matching paren
(show-paren-mode 1)

;; Remove trailing whitespaces before save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No backups!
(setq make-backup-files nil)

;; Mo #Deamned-auto-save-trash#
(setq auto-save-default nil)

;; Enable commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Don't ever use arrows!!!
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))


;; Smart home
(defun My-smart-home () "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (and (eq last-command 'My-smart-home)
	   (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text)))

(global-set-key [home] 'My-smart-home)

(provide 'emacs-behaviour)

(require 'automargin)
(setq automargin-target-width 100)
(automargin-mode 1)


;; Use long names instead of <N>s.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Nice window switching
(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)


;; Revert all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
		 (file-exists-p (buffer-file-name))
		 (not (buffer-modified-p)))
	(revert-buffer t t t))))
  (message "Refreshed open files."))
