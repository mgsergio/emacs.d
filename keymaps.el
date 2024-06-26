;;; -*- lexical-binding: t; -*-
(require 'dash)

(defun symbol-maybe-value (sym)
  (when (boundp sym)
    (symbol-value sym)))

(defun relaxed-keymap-value (symbol-or-keymap &rest args)
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
            (apply #'relaxed-keymap-value it args)))

    ((and (pred boundp)
          (pred (lambda (x) (-> x symbol-value keymapp))))
     (-as-> symbol-or-keymap it
            (symbol-value it)
            (apply #'relaxed-keymap-value it args)))

    (_
     (unless (plist-get args :nil-on-error)
       (error "%S is not a keymap" symbol-or-keymap)))))

(defun relaxed-keymap-p (maybe-keymap-or-sym)
  (relaxed-keymap-value maybe-keymap-or-sym
                        :nil-on-error t))

(defun get-known-keymaps ()
  (let (maps)
    (mapatoms (lambda (x)
                (when (relaxed-keymap-p x)
                  (push x maps))))
    (seq-sort #'string< maps)))

;; (get-known-keymaps)
;; (relaxed-keymap-p :conc-name)
;; (relaxed-keymap-p 'isearch-pre-move-point)

(defun walk-keymap (keymap)
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
                                      result)))))))

    ;; Push the first target to the stack/queue.
    (push (list :keymap keymap
                :path start-path)
          next)

    (while-let ((current-item (pop next)))
      (let ((current-map (plist-get current-item :keymap))
            ;; (not-seen-guard (not (memq current-map seen)))
            ;; (keymap-value (relaxed-keymap-value current-map))
            (current-path (plist-get current-item :path)))

        (dolist (entry (-> current-map
                           relaxed-keymap-value
                           cdr))
          (pcase entry
            ;; chat-table. Will traverse. TODO:
            ((pred char-table-p) (message "char-table"))

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

            ;; menu-item. Ignore.
            (`(,_e menu-item . ,_d)
             nil)

            ;; A keymap
            (`(,e keymap . ,b)
             (funcall handle-binding e `(keymap ,@b) current-path)
             ;; (message "binding + menu-item")
             )

            ;; Simple menu item and a binding. Ignore
            (`(,e ,_ . ,b)
             ;; (funcall handle-binding e b current-path)
             ;; (message "binding + menu-item")
             )

            ;; Default binding.
            (`(t . ,b)
             (funcall handle-binding t b current-path)
             ;; (message "default binding")
             )

            ;; The simplest binding.
            (`(,e . ,b)
             (funcall handle-binding e b current-path))))
        ))
    result))


;;; Playground:
;;
;; (pp-eval-expression '(take 10000
;;                            (seq-reverse
;;                             (walk-keymap 'global-map))))
;;
;; (walk-keymap 'Buffer-menu-mode-menu)
;;
;; (pp-eval-expression
;;  '(walk-keymaps (get-known-keymaps)))
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

;; Local Variables:
;; eval: (flycheck-disable-checker 'emacs-lisp-checkdoc)
;; eval: (smartparens-mode -1)
;; eval: (electric-pair-mode t)
;; End:
