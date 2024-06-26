;;; -*- lexical-binding: t; -*-

(require 'dash)

(defun symbol-maybe-value (sym)
  (when (boundp sym)
    (symbol-value sym)))

(defun relaxed-keymap-value (symbol-or-keymap &rest args)
  (pcase symbol-or-keymap
    ;; ------------------------------ HERE --------------------------------------
    ;; ((pred autoloadp)
    ;;  (autoload-do-load symbol-or-keymap)
    ;;  (apply #'relaxed-keymap-value symbol-or-keymap args))

    ;; ((pred keywordp)
    ;;  (unless (plist-get args :nil-on-error)
    ;;    (error "%S is not a keymap" symbol-or-keymap)))

    ((pred (not symbolp))
     (if (keymapp symbol-or-keymap)
         symbol-or-keymap
       (unless (plist-get args :nil-on-error)
         (error "%S is not a keymap" symbol-or-keymap))))

    ((pred keymapp)
     (when (-> symbol-or-keymap symbol-function autoloadp)
       (autoload-do-load symbol-or-keymap))
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

(defun walk-keymap (keymap &optional start-path)
  (let* ((next '())
         seen
         result
         (handle-binding (lambda (e b path)
                           (message "e: %S\nb: %S\npath: %s" e b path)
                           (let* ((new-path (cons e path))
                                  (rec (list :keymap keymap
                                             :target b
                                             :sequence new-path)))
                             ;; (when (eql e 8388720)
                             ;;   (message "GOT THE BINDING OF MY DREAMS: %S" b))
                             (pcase b
                               ((pred keymapp)
                                (message "KEYMAP FOUND!")

                                (if (symbolp b)
                                    (progn
                                      (message "KEYMAP IS A SYMBOL!")
                                      (push (plist-put rec :final nil)
                                            result)
                                      (unless (memq b seen)
                                        (push (list :keymap b
                                                    :path new-path)
                                              next)))
                                  (message "Pushing down a keymap that is NOT a symbol!")
                                  (push (list :keymap b
                                              :path new-path)
                                        next)
                                  ;; (message "NEXT not looks like this: %S" (take 10 next))
                                  (when (eql 8388720 e)
                                    (push (plist-put (pop next)
                                                     :dreamy t)
                                          next))
                                  ))

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

    ;; (message "Before while-let")
    ;; (message "next: %S" next)

    (while-let ((current-item (pop next)))
      (message "In while-let")
      (let ((current-map (plist-get current-item :keymap))
            ;; (not-seen-guard (not (memq current-map seen)))
            ;; (keymap-value (relaxed-keymap-value current-map))
            (current-path (plist-get current-item :path)))

        (unless (memq current-map seen)

          (when (plist-get current-item :dreamy)
            (message "HACK ACTIVATED"))

          ;; (message "========")
          ;; (message "current-item: %S" current-item)
          ;; (message "current-map: %S" current-map)
          ;; (message "========")

          (message "Before unless")

          (unless (listp (-> current-map
                             relaxed-keymap-value
                             cdr))
            (message "Got smth strange: %S" current-map))

          (message "Before dolist")
          (dolist (entry (-> current-map
                             relaxed-keymap-value
                             cdr))
            (message "In dolist")
            (pcase entry
              ;; chat-table. Will traverse.
              ((pred char-table-p) (message "char-table"))

              ;; vector. The keymap is most likely a menu map. Should I ignore it?
              ((pred vectorp)
               (message "vector"))

              ;; A nested keymap. Traverse. If a symbol, also remember the connection.
              ((pred keymapp)
               (push (list :keymap keymap
                           :path current-path)
                     next))

              ;; Ingore
              ((pred stringp)
               (message "string")
               nil)

              ;; menu-item. Ignore.
              (`(,_e menu-item . ,_d)
               (message "menu-item")
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
               (funcall handle-binding e b current-path)
               ;; (message "binding %S: %S" e (type-of e))
               )))
          (when (symbolp current-map)
            (push current-map seen))
          )))
    (message "SEEN MAPTS: %S" seen)
    result))

;; (pp-eval-expression '(take 10000
;;                            (seq-reverse
;;                             (walk-keymap 'global-map))))

(walk-keymap 'Buffer-menu-mode-menu)


;; (defun my-group-by (seq)
;;   (let ((table '()))
;;     (dolist (item seq)
;;       (if-let ((target (plist-get item :target))
;;                (sequence (plist-get item :sequence))
;;                (entry (assoc target table)))
;;           (push sequence (cdr entry))
;;         (push (cons target (list sequence))
;;               table)))
;;     table))


;; (pp-eval-expression
;;  '(seq-filter (lambda (x) (> (length x) 2))
;;               (my-group-by (walk-keymap 'global-map)))
;;  )


(dolist (map (get-known-keymaps))
  (message "Handling: %S" map)
  (walk-keymap map))

(walk-keymap 'gnus-summary-score-map)
(relaxed-keymap-p (symbol-function 'gnus-summary-score-map))
(relaxed-keymap-p 'gnus-summary-score-map)
(relaxed-keymap-value (symbol-function 'gnus-summary-score-map))
(relaxed-keymap-value 'gnus-summary-score-map :nil-on-error t)
(keymapp 'gnus-summary-score-map)
(keymapp (symbol-function 'gnus-summary-score-map))
(autoloadp 'gnus-summary-score-map)

(autoload-do-load (symbol-function 'gnus-summary-score-map))




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


;; (seq-filter (lambda (x) (> (length (plist-get x :sequence))) 2)
;;             (walk-keymap 'global-map))

;; (key-description '(8388720 8388722))


;;; Playground:

;; (pp-eval-expression
;;  '(let* ((b (seq-find (lambda (x) (and (consp x)
;;                                       (eql (car x)
;;                                            8388720)))
;;                      global-map))
;;         (m (make-symbol "m")))

;;    (setf (symbol-function m)
;;          (list 'keymap b))
;;    (walk-keymap m)
;;    )
;; )


;; (autoloadp (symbol-function '2C-command))
;; (keymapp '2C-command)
;; (autoloadp #'2C-command)
;;
;; (symbol-function  'help-command)
;;
;; (seq-filter (lambda (x) (and (symbolp x)
;;                              ;; (relaxed-keymap-p x))
;;                              ))
;;             global-map)

;; (length (get-known-keymaps))
;; (length (seq-filter #'keymapp (get-known-keymaps)))
;;

;; Local Variables:
;; eval: (flycheck-disable-checker 'emacs-lisp-checkdoc)
;; eval: (smartparens-mode -1)
;; eval: (electric-pair-mode t)
;; End:
