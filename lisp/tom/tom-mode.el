;; One Mode to rule them all.
;; A (yet another) self-made modal editing mode.

(require 'cl-lib)
(require 'dash)

(require 'tom-keys)
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

(defvar tom-normal-state-map)
(let ((m (make-keymap)))
  (suppress-keymap m)
  (tom--populate-motion-map m)
  (tom--populate-control-map m)
  (tom--populate-text-manipulation-map m)
  (setf tom-normal-state-map m))

(defvar tom-select-state-map nil)
(defvar tom-line-select-state-map nil)

(defvar tom-insert-state nil)
(defvar tom-select-sate nil)
(defvar tom-line-select-state nil)
(defvar tom-normal-state nil)

;; Order metters.
(defvar tom--map-alist
  `(;; (tom-insert-state . tom-insert-state-map)
    (tom-select-state . ,tom-select-state-map)
    (tom-line-select-stste . ,tom-line-select-state-map)
    (tom-normal-state . ,tom-normal-state-map)))

(defun tom--enter-normal-state ()
  (setf cursor-type 'box)
  (setf tom-normal-state t))

;; TODO: Should be insert mode or smth.
(defun tom--leave-normal-state ()
  (setf cursor-type 'bar))

;; (if (not cua-mode)
;;     (setq emulation-mode-map-alists
;;	  (delq 'cua--keymap-alist emulation-mode-map-alists))
;;   (add-to-ordered-list 'emulation-mode-map-alists 'cua--keymap-alist 400)
;;   (cua--select-keymaps))

(define-minor-mode tom-mode
  "A minor mode for modal editing"
  :init-value nil
  ;; TODO(mgserjio): Think on it again.
  :lighter "TOM"
  (if tom-mode
      (progn
	(add-to-list 'emulation-mode-map-alists 'tom--map-alist)
	(tom--enter-normal-state))
    (delq 'tom--map-alist emulation-mode-map-alists)
    (tom--leave-normal-state)))

;; TODO: use defcustom instead.
(defvar tom-major-mode-white-list nil)
(defvar tom-buffer-name-white-list nil)

(defvar tom-major-mode-black-list (list "^helm.*" "^magit.*"))
(defvar tom-buffer-name-black-list (list "\*Minibuf-\d+\*"))

(defun tom--check-white-lists ()
  (or (cl-member-if (lambda (x) (string-match-p x (symbol-name major-mode)))
		     tom-major-mode-white-list)
      (cl-member-if (lambda (x) (string-match-p x (buffer-name)))
		    tom-buffer-name-white-list)))

(defun tom--check-black-lists ()
  (not (or (cl-member-if (lambda (x) (string-match-p x (symbol-name major-mode)))
			 tom-major-mode-black-list)
	   (cl-member-if (lambda (x) (string-match-p x (buffer-name)))
			 tom-buffer-name-black-list))))

(defun tom--check-buffer ()
  (if (or tom-major-mode-white-list
	  tom-buffer-name-white-list)
      (tom--check-wite-lists)
    (tom--check-black-lists)))

(defun turn-on-tom-mode ()
  (when (tom--check-buffer)
    (tom-mode 1)))

(define-globalized-minor-mode tom-global-mode
  tom-mode
  turn-on-tom-mode)

(provide 'tom-mode)
