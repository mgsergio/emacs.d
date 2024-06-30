;;; -*- lexical-binding: t; -*-
(require 'dash)

(defun pathfinder-symbol-maybe-value (sym)
  (when (boundp sym)
    (symbol-value sym)))

(defun pathfinder-relaxed-keymap-value (symbol-or-keymap &rest args)
  (pcase symbol-or-keymap
    ((pred (not symbolp))
     (if (keymapp symbol-or-keymap)
         symbol-or-keymap
       (unless (plist-get args :nil-on-error)
         (error "%S is not a keymap" symbol-or-keymap))))

    ((pred keymapp)
     (when-let ((maybe-autoload (symbol-function symbol-or-keymap))
                (is-autoload (autoloadp maybe-autoload)))
       (autoload-do-load maybe-autoload symbol-or-keymap))
     (-as-> symbol-or-keymap it
            (symbol-function it)
            (apply #'pathfinder-relaxed-keymap-value it args)))

    ((and (pred boundp)
          (pred (lambda (x) (-> x symbol-value keymapp))))
     (-as-> symbol-or-keymap it
            (symbol-value it)
            (apply #'pathfinder-relaxed-keymap-value it args)))

    (_
     (unless (plist-get args :nil-on-error)
       (error "%S is not a keymap" symbol-or-keymap)))))

(defun pathfinder-relaxed-keymap-p (maybe-keymap-or-sym)
  (pathfinder-relaxed-keymap-value maybe-keymap-or-sym
                                   :nil-on-error t))

(defun pathfinder-get-known-keymaps ()
  (let (maps)
    (mapatoms (lambda (x)
                (when (pathfinder-relaxed-keymap-p x)
                  (push x maps))))
    (seq-sort #'string< maps)))

;; (pathfinder-get-known-keymaps)
;; (relaxed-keymap-p :conc-name)
;; (relaxed-keymap-p 'isearch-pre-move-point)

(defun pathfinder-walk-keymap (keymap)
  (let* ((next '())
         (result '())
         (start-path '())
         (handle-binding (lambda (e b path)
                           (let* ((new-path (cons e path))
                                  (rec (list :keymap keymap
                                             :target b
                                             :sequence new-path)))
                             (pcase b
                               ((pred keymapp)
                                (if (symbolp b)
                                    (progn
                                      (push (plist-put rec :final nil)
                                            result))
                                  (push (list :keymap b
                                              :path new-path)
                                        next)))

                               ((pred commandp)
                                (push (plist-put rec :final t)
                                      result))

                               ((pred not)
                                (push (plist-put rec :final t)
                                      result))

                               (_
                                (warn "Unexpected binding found: %S %S" e b)))))))

    ;; Push the first target to the stack/queue.
    (push (list :keymap keymap
                :path start-path)
          next)

    (while-let ((current-item (pop next)))
      (let ((current-map (plist-get current-item :keymap))
            (current-path (plist-get current-item :path)))

        (dolist (entry (-> current-map
                           pathfinder-relaxed-keymap-value
                           cdr))
          (pcase entry
            ;; chat-table. Will traverse.
            ((pred char-table-p)
             (map-char-table (lambda (e b)
                               (if (consp e)
                                   (funcall handle-binding
                                            `(,(car e) . ,(cdr e))  ;; Copy to avoid capturing by reference.
                                            b current-path)
                                 (funcall handle-binding e b current-path)))
                             entry))

            ;; vector. The keymap is most likely a menu map. Should I ignore it?
            ((pred vectorp)
             nil)

            ;; A nested keymap. Traverse.
            ((pred keymapp)
             (push (list :keymap entry
                         :path current-path)
                   next))

            ;; Ingore
            ((pred stringp)
             nil)

            ;; A keymap
            (`(,e keymap . ,b)
             (funcall handle-binding e `(keymap ,@b) current-path)
             )

            ;; menu-item. Ignore.
            (`(,_e menu-item . ,_d)
             nil)

            ;; Simple menu item and a binding. Ignore
            ;; TODO: I guess this one is never called.
            (`(,e ,_ . ,b)
             ;; (funcall handle-binding e b current-path)
             )

            ;; Default binding.
            (`(t . ,b)
             (funcall handle-binding t b current-path))

            ;; The simplest binding that is either a command, or some weird stuff.
            ;; TODO: Should I only alow proper commands?
            ;;       Or handle the "stuff" properly?
            (`(,e . ,b)
             (funcall handle-binding e b current-path))))
        ))
    result))

(defun pathfinder-walk-keymaps (keymaps)
  (let ((results '()))
    (dolist (keymap keymaps)
      (push (pathfinder-walk-keymap keymap)
            results))
    (apply #'append results)))


;;; Playground:
;;
;; (pp-eval-expression '(take 10000
;;                            (seq-reverse
;;                             (walk-keymap 'global-map))))
;;
;; (pathfinder-walk-keymap 'Buffer-menu-mode-menu)
;;
;; (pp-eval-expression
;;  '(pathfinder-walk-keymaps (pathfinder-get-known-keymaps)))
;;

;;; Interesting cases:
;;
;; I
;; This is an example of a symbol having another symbol in a value cell, that
;; has a keymap in its function cell.
;; (symbol-function (symbol-value 'cider-clojure-mode-menu-open))
;; (relaxed-keymap-value 'cider-clojure-mode-menu-open)
;;
;; II
;; (symbol-function 'gnus-summary-score-map) contains an autoload.
;;
;; (autoloadp (symbol-function '2C-command)) another example
;; (keymapp '2C-command)
;; (autoloadp #'2C-command)
;;
;;; END: interesting cases.

(provide 'pathfinder)
;;; pathfinder.el ends here.

;; Local Variables:
;; eval: (flycheck-disable-checker 'emacs-lisp-checkdoc)
;; eval: (smartparens-mode -1)
;; eval: (electric-pair-mode t)
;; End:
