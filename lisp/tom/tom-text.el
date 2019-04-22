;;;;;;;;;;;;;
;; Objects ;;
;;;;;;;;;;;;;

;; TODO: These functions share a lot of code, consider
;; refactoring.

(defun tom--get-word-forward ()
  (let ((curr (point))
	(end (save-excursion
	       (forward-word)
	       (point))))
	(cons curr end)))

(defun tom--get-word-backward ()
  (let ((curr (point))
	(end (save-excursion
	       (backward-word)
	       (point))))
    (cons end curr)))

(defun tom--get-word-full ()
    (bounds-of-thing-at-point 'word))


;; TODO: Make it respect DWM pricipal. I.E. include \n as well if
;; line is empty. Or make separate versions one that incude \n
;; and one that excudes.
(defun tom--get-line-forward ()
  (let ((curr (point))
	(end (save-excursion
	       (forward-thing 'line)
	       (- (point) 1))))
	(cons curr end)))

(defun tom--get-line-backward ()
  (let ((curr (point))
	(end (save-excursion
	       (beginning-of-thing 'line)
	       (point))))
    (cons end curr)))

(defun tom--get-line-full ()
    (bounds-of-thing-at-point 'line))

;;;;;;;;;;;;;;;;
;; Operations ;;
;;;;;;;;;;;;;;;;

;; Killing

(defun tom-kill-word-forward ()
  (interactive)
  (let ((region (tom--get-word-forward)))
    (kill-region (car region)
		 (cdr region))))

(defun tom-kill-word-backward ()
  (interactive)
  (let ((region (tom--get-word-backward)))
    (kill-region (car region)
		 (cdr region))))

(defun tom-kill-word-full ()
  (interactive)
  (let ((region (tom--get-word-full)))
    (kill-region (car region)
		 (cdr region))))

(defun tom-kill-line-forward ()
  (interactive)
  (let ((region (tom--get-line-forward)))
    (kill-region (car region)
		 (cdr region))))

(defun tom-kill-line-backward ()
  (interactive)
  (let ((region (tom--get-line-backward)))
    (kill-region (car region)
		 (cdr region))))

(defun tom-kill-line-full ()
  (interactive)
  (let ((region (tom--get-line-full)))
    (kill-region (car region)
		 (cdr region))))

;; Yanking

(defun tom-yank-word-forward ()
  (interactive)
  (let ((region (tom--get-word-forward)))
    (kill-ring-save (car region)
		    (cdr region))))

(defun tom-yank-word-backward ()
  (interactive)
  (let ((region (tom--get-word-backward)))
    (kill-ring-save (car region)
		    (cdr region))))

(defun tom-yank-word-full ()
  (interactive)
  (let ((region (tom--get-word-full)))
    (kill-ring-save (car region)
		    (cdr region))))

(defun tom-yank-line-forward ()
  (interactive)
  (let ((region (tom--get-line-forward)))
    (kill-ring-save (car region)
		    (cdr region))))

(defun tom-yank-line-backward ()
  (interactive)
  (let ((region (tom--get-line-backward)))
    (kill-ring-save (car region)
		    (cdr region))))

(defun tom-yank-line-full ()
  (interactive)
  (let ((region (tom--get-line-full)))
    (kill-ring-save (car region)
		    (cdr region))))


;; Inserting

(defun tom-insert-newline-above ()
  (interactive)
  (let ((line-start (beginning-of-thing 'line)))
    (save-excursion
      (goto-char line-start)
      (newline))))

(defun tom-insert-newline-below ()
  (interactive)
  (let ((line-end (end-of-thing 'line)))
      (goto-char line-end)
      (newline)))

(provide 'tom-text)

;; sdkfjsldf
;; sdfs
;; d
;; sdf
;; sdfsdf lskdjf sdf odfjsd
;; f skdf slfj ewojf wo3r3u 40wslfij 4
;; f djflgjwr pq sfj vlckvm ;wt wrigj eorigu
