;;; -*- lexical-binding: t; -*-

(require 'dash)

(defun symbol-maybe-value (sym)
  (when (boundp sym)
    (symbol-value sym)))

(defun relaxed-keymap-p (keymap-or-sym)
  (or (keymapp keymap-or-sym)
      (keymapp (symbol-maybe-value keymap-or-sym))))

(defun relaxed-keymap-value-ex (symbol-or-keymap)
  (unless (relaxed-keymap-p symbol-or-keymap)
    (error "%S is not a keymap" symbol-or-keymap))
  (if (symbolp symbol-or-keymap)
      (or (symbol-function symbol-or-keymap)
          (symbol-value symbol-or-keymap))
    symbol-or-keymap))

(defun relaxed-keymap-value (symbol-or-keymap)
  (unless (relaxed-keymap-p symbol-or-keymap)
    (error "%S is not a keymap" symbol-or-keymap))
  (pcase symbol-or-keymap
    ((pred (not symbolp))
     symbol-or-keymap)

    ((pred keymapp)
     (-> symbol-or-keymap
         symbol-function
         relaxed-keymap-value))

    (_
     (-> symbol-or-keymap
         symbol-value
         relaxed-keymap-value))
    ;; (symbol-value symbol-or-keymap))))
    ))

(defun get-known-keymaps ()
  (let (maps)
    (mapatoms (lambda (x)
                (when (relaxed-keymap-p x)
                  (push x maps))))
    (seq-sort #'string< maps)))


(length (get-known-keymaps))
(length (seq-filter #'keymapp (get-known-keymaps)))

;;


;; (cl-loop for x in '(1 2 3)
;;          for y in '(4 5 6)
;;          collect `(,x . ,y))

;; current path:: string
;; command -> [(keymap path . is-global)]


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
               (if (symbolp entry)
                   (message "keymap symbol")
                 (message "keymap value")))

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

(keymapp menu-function-47)
(symbol-function 'menu-function-47)
(symbol-function 'menu-function-47)
(symbol-function (symbol-value 'cider-clojure-mode-menu-open))

(relaxed-keymap-value 'cider-clojure-mode-menu-open)


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


;; Local Variables:
;; eval: (flycheck-disable-checker 'emacs-lisp-checkdoc)
;; eval: (progn
;;         (smartparens-mode -1)
;;         (electric-pair-mode t))
;; End:
