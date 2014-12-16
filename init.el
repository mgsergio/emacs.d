(require 'package)
(package-initialize) ; force all packages to load

;; My Emacs settings
(add-to-list 'load-path "~/.emacs.d/init.d")

;; Custom styles
(add-to-list 'load-path "~/.emacs.d/styles")

;; Add repos with lots of cool packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(require 'look-and-feel)
(require 'emacs-behaviour)

;; Toggle cua-mode
(cua-mode)
(setq cua-enable-cua-keys nil)  ;; Disable cua c-x/c-c/c-v bindings


;; Dear speedbar, please show me all files,
;; not just those you know
(custom-set-variables
 '(speedbar-show-unknown-files t))

(defun my-sr-speedbar-toggle-and-select ()
  (interactive)
  (sr-speedbar-toggle)
  (sr-speedbar-select-window))

;; Toggle speedbar with F12
(global-set-key (kbd "<f12>") 'my-sr-speedbar-toggle-and-select)


;; The only way of doing things!
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq
;; helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
 helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
 helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
 helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf t)



;; TODO: Move to behaviour
(move-text-default-bindings)
(fullscreen-mode)

;; Setup cider, a repl mode for clojure, and more
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (setq nrepl-hide-special-buffers t) ;; hiding special buffers
;; (etq cider-repl-tab-command 'indent-for-tab-command) ;; tab ke behaviour in repl


;; cc-mode Settings
(add-to-list 'auto-mode-alist
	     '("\\.h\\'" . c++-mode))  ; C++-mode for .h files

(require 'metrika-c++-style)
(add-hook 'c-initialization-hook 'metrika-set-c++-style)

;; Useful key bindings
(global-set-key (kbd "<f5>") 'compile)


;; TODO: Use helm-projectile
