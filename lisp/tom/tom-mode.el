;; One Mode to rule them all.
;; A (yet another) self-made modal editing mode.

(require 'cl-lib)
(require 'dash)

(require 'tom-text)

;; This is a reference of some commands tha should be rebind
;; when helm/counsel/ido are enabled.
;; execute-extended-command        counsel-M-x
;; describe-bindings               counsel-descbinds
;; describe-function               counsel-describe-function
;; describe-variable               counsel-describe-variable
;; apropos-command                 counsel-apropos
;; describe-face                   counsel-describe-face
;; list-faces-display              counsel-faces
;; find-file                       counsel-find-file
;; find-library                    counsel-find-library
;; imenu                           counsel-imenu
;; load-library                    counsel-load-library
;; load-theme                      counsel-load-theme
;; yank-pop                        counsel-yank-pop
;; info-lookup-symbol              counsel-info-lookup-symbol
;; pop-to-mark-command             counsel-mark-ring
;; bookmark-jump                   counsel-bookmark

;; (defvar tom--helm-control-fucntuons
;;   '(:M-x 'helm-M-x
;;     :find-file 'helm-find-files
;;     :switch-to-buffer 'helm-mini))

;; (defvar tom--ivi-control-function
;;   (:M-x

(defun tom--define-keys (m &rest defs)
  (when  (-> defs length cl-oddp)
    (error "defs should be a full alist."))
  (cl-flet ((vectorize (seq)
		       (if (stringp seq)
			   (kbd seq)
			 seq)))
    (cl-loop for (seq def) on defs
	     by 'cddr
	     do (define-key m (vectorize seq) def))))


(defun tom--populate-motion-map (m)
  (tom--define-keys m
		    "e" 'previous-line
		    "E" (lambda ()
			  (interactive)
			  (previous-line 7))
		    "d" 'next-line
		    "D" (lambda ()
			  (interactive)
			  (next-line 7))

		    "s" 'backward-word
		    "f" 'forward-word
		    "S" 'backward-char
		    "F" 'forward-char

		    "w" 'beginning-of-line-text
		    "W" 'beginning-of-line
		    "r" 'end-of-line

		    "/" 're-search-forward
		    "?" 're-search-backward

		    ;; TODO: Make it smakrt...
		    "m" 'set-mark-command))

(defun tom--populate-text-manipulation-map (m)
  (tom--define-keys m
		    ;; TODO "k t *" -- kill `thing'
		    ;; TODO "y t *" -- yank `thing'
		    "k k" 'tom-kill-line-full
		    "k f" 'tom-kill-word-forward
		    "k s" 'tom-kill-word-backward
		    "k r" 'tom-kill-line-forward
		    "k w" 'tom-kill-line-backward

		    "y y" 'tom-yank-line-full
		    "y f" 'tom-yank-word-forward
		    "y s" 'tom-yank-word-backward
		    "y r" 'tom-yank-line-forward
		    "y w" 'tom-yank-line-backward

		    "I e" 'tom-insert-line-above-and-insert-state
		    "I d" 'tom-insert-line-below-and-insert-state

		    "l e" 'tom-insert-newline-above
		    "l d" 'tom-insert-newline-below

		    "p" 'yank
		    "x" 'kill-region
		    ))

(defun tom--populate-control-map (m)
  (tom--define-keys m
		    "SPC SPC" 'helm-M-x
		    "SPC f f" 'helm-find-files
		    "SPC f s" 'save-buffer
		    "SPC b b" 'helm-mini
		    "SPC b B" 'ibuffer

		    "SPC p p" 'projectile-switch-project
		    "SPC p f" 'projectile-find-file

		    "i w" (lambda ()
			    (interactive)
			    (beginning-of-line-text)
			    (tom-mode -1))
		    "i W" (lambda ()
			    (interactive)
			    (beginning-of-line)
			    (tom-mode -1))
		    "i r" (lambda ()
			    (interactive)
			    (end-of-line)
			    (tom-mode -1))
		    "i i" 'tom-mode
		    "0" 'tom-mode))

(defvar tom-keymap)
(let ((m (make-keymap)))
  (suppress-keymap m)
  (tom--populate-motion-map m)
  (tom--populate-control-map m)
  (tom--populate-text-manipulation-map m)
  (setf tom-keymap m))

(global-set-key (kbd "C-]") 'tom-mode)

(define-minor-mode tom-mode
  "A minor mode for modal editing"
  :init-value nil
  ;; TODO(mgserjio): thing on it again.
  :lighter "TOM"
  :keymap tom-keymap)

(provide 'tom-mode)
