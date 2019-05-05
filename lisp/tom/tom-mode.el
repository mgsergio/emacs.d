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


(defvar tom-keymap)
(let ((m (make-keymap)))
  (suppress-keymap m)
  (tom--populate-motion-map m)
  (tom--populate-control-map m)
  (tom--populate-text-manipulation-map m)
  (setf tom-keymap m))

;; TODO: Should be normal mode or smth.
(defun tom--init ()
  (setf cursor-type 'box))

;; TODO: Should be insert mode or smth.
(defun tom--deinit ()
  (setf cursor-type 'bar))

(define-minor-mode tom-mode
  "A minor mode for modal editing"
  :init-value nil
  ;; TODO(mgserjio): Think on it again.
  :lighter "TOM"
  :keymap tom-keymap
  (if tom-mode
      (tom--init)
    (tom--deinit)))

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
