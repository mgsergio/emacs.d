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

            ;; `'(,e (pred stringp) . ,b) -- a simple menu item.
            ;; e is most likelly a symbol.

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




;;; Tests:
(ert-deftest test-pathfinder-relaxed-keymap-value ()
  ;; By value.
  (should (equal (make-keymap)
                 (pathfinder-relaxed-keymap-value (make-keymap))))

  ;; Variable holding a value.
  (let ((km (make-keymap)))
    (should (eq km
                (pathfinder-relaxed-keymap-value km))))

  ;; A keymap in a function cell.
  (let ((sym (gensym)))
    (setf (symbol-function sym) (make-keymap))
    (should (eq (symbol-function sym)
                (pathfinder-relaxed-keymap-value sym))))

  ;; A keymap in a value cell.
  (let ((sym (gensym)))
    (setf (symbol-value sym) (make-keymap))
    (should (eq (symbol-value sym)
                (pathfinder-relaxed-keymap-value sym))))

  ;; A symbol pointing to another symbol with a keymap.
  (let ((outer-sym-fn (gensym))
        (outer-sym-val (gensym))
        (inner-sym (gensym)))
    (setf (symbol-function inner-sym) (make-keymap)
          (symbol-function outer-sym-fn) inner-sym
          (symbol-value outer-sym-val) inner-sym)
    (should (eq (symbol-function inner-sym)
                (pathfinder-relaxed-keymap-value outer-sym-fn)))
    (should (eq (symbol-function inner-sym)
                (pathfinder-relaxed-keymap-value outer-sym-val))))

  ;; An autoload a symbol-fynction
  (let* ((sym 'pathfinder--test-autoload-keymap)
         (tmp-file-name (make-temp-file "pathfiner-test-fixtures.el"
                                        nil ; DIR-FLAG
                                        ".el" ; SUFFIX
                                        (format "(fset '%s (make-keymap))\n" (symbol-name sym)))))
    (unwind-protect
        (progn
          (autoload sym
            tmp-file-name
            nil
            nil
            'keymap)

          (cl-assert (autoloadp (symbol-function sym)))
          (cl-assert (keymapp sym))

          (let ((resulting-keymap (pathfinder-relaxed-keymap-value sym)))
            (should (symbol-function sym))
            (should (eq (symbol-function sym)
                        resulting-keymap))))
      (unintern sym nil)
      (delete-file tmp-file-name))))

(ert-deftest test-pathfinder-walk-keymap ()
  ;; Trivial
  (should (equal (pathfinder-walk-keymap (make-keymap))
                 '()))

  ;; A couple of normal bindings.
  (let ((map (make-sparse-keymap)))
    (keymap-set map "i" #'self-insert-command)
    (keymap-set map "f" #'find-file)
    (should (equal (pathfinder-walk-keymap map)
                   (list (list :keymap map
                               :target #'self-insert-command
                               :sequence `(,?i)
                               :final t)
                         (list :keymap map
                               :target #'find-file
                               :sequence `(,?f)
                               :final t)))))

  ;; A char-table is handled properly
  (let ((map (make-keymap)))
    (keymap-set map "a" 'self-insert-command)
    (keymap-set map "b" 'self-insert-command)
    (keymap-set map "c" 'self-insert-command)
    ;; d is intentionally skipped to create two intervals.
    (keymap-set map "e" 'self-insert-command)
    (keymap-set map "f" 'self-insert-command)
    (keymap-set map "g" 'self-insert-command)

    (cl-assert (equal (length map) 2))

    (should (seq-set-equal-p (pathfinder-walk-keymap map)
                             (list (list :keymap map
                                         :target #'self-insert-command
                                         :sequence `((?a . ?c))
                                         :final t)
                                   (list :keymap map
                                         :target #'self-insert-command
                                         :sequence `((?e . ?g))
                                         :final t)))))

  ;; Nested maps
  (let ((inner-map-1 (make-sparse-keymap))
        (inner-map-2 (make-sparse-keymap))
        (outer-map (make-sparse-keymap)))
    (keymap-set inner-map-1 "i" #'self-insert-command)
    (keymap-set inner-map-2 "f" #'find-file)
    (keymap-set outer-map "h" #'help)
    ;; NOTE: I'm not using make-comosed-keymap intentionally,
    ;;       because I want the exact structure of my keymap:
    ;;       (keymap binding (keymap ...) (keymap ...))
    (setf outer-map
          (append outer-map
                  (list inner-map-1
                        inner-map-2)
                  nil))
    (should (seq-set-equal-p (pathfinder-walk-keymap outer-map)
                             (list (list :keymap outer-map
                                         :target #'help
                                         :sequence `(,?h)
                                         :final t)
                                   (list :keymap outer-map
                                         :target #'self-insert-command
                                         :sequence `(,?i)
                                         :final t)
                                   (list :keymap outer-map
                                         :target #'find-file
                                         :sequence `(,?f)
                                         :final t)))))

  ;; Recursive
  (let ((inner-map-1 (make-sparse-keymap))
        (inner-map-2 (make-sparse-keymap))
        (outer-map (make-sparse-keymap)))
    (keymap-set inner-map-1 "i" #'self-insert-command)
    (keymap-set inner-map-2 "f" #'find-file)
    (keymap-set outer-map "h" #'help)
    ;; NOTE: I'm not using make-comosed-keymap intentionally,
    ;;       because I want the exact structure of my keymap:
    ;;       (keymap binding (keymap ...
    ;;                               (keymap ...)))
    (setf inner-map-2
          (append inner-map-2 (list inner-map-1) nil)
          outer-map
          (append outer-map (list inner-map-2) nil))
    (should (seq-set-equal-p (pathfinder-walk-keymap outer-map)
                             (list (list :keymap outer-map
                                         :target #'help
                                         :sequence `(,?h)
                                         :final t)
                                   (list :keymap outer-map
                                         :target #'self-insert-command
                                         :sequence `(,?i)
                                         :final t)
                                   (list :keymap outer-map
                                         :target #'find-file
                                         :sequence `(,?f)
                                         :final t)))))

  ;; Prefix key
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-x f" #'find-file)
    (should (equal (pathfinder-walk-keymap map)
                   (list (list :keymap map
                               :target #'find-file
                               :sequence '(?f ?\C-x)
                               :final t)))))

  ;; TODO:
  ;; - [ ] Parent map
  ;; - [ ] An innder kymap symbol in an outer keymap should
  ;;       result in a :map set to inner keymap. Really???
  )


;; To tun tests with latest changes:
;;   (progn (eval-buffer) (ert t))
;;
;;; END: Tests


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



(defun pathfinder--tag-keymap-items (item)
  (list :item item
        :type
        (pcase item
          ((or (pred vectorp)
               (pred char-table-p)
               (pred stringp))
           (type-of item))

          ((pred keymapp)
           'keymap)

          (`(,_ menu-item . ,_)
           'menu-item)

          (`(,_ ,(pred stringp) . ,_)
           'simple-menu-item)

          (`(,_ . ,(pred commandp))
           'command)

          (`(,_ . ,(pred keymapp))
           'prefix)

          (_ 'unknown))))


(ert-deftest test-pathfinder--tag-keymap-items ()
  (pcase-dolist (`(,input . ,type)
                 `(("Hello" . string)
                   (,(make-char-table 'test) . char-table)
                   ([1 2 3] . vector)

                   ((event menu-item bla bla bla) . menu-item)
                   ((event "foo" bla bla bla) . simple-menu-item)

                   ((?h . find-file) . command)
                   ((?h . "Hello") . command)
                   ((?h . [1 2 3]) . command)

                   ((?h . ,(make-sparse-keymap)) . prefix)

                   (123 . unknown)
                   ((?h . (lambda ())) . unknown)))

    (should (equal (pathfinder--tag-keymap-items input)
                   (list :item input
                         :type type)))))


(defun pathfinder--make-parent-keymap-repack-and-tag-xf ()
  (lambda (rf)
    (let ((first-keymap-sym-seen nil)
          (collecting-parent nil)
          (parent-keymap '()))

      (lambda (&rest args)
        (pcase args
          ('() (funcall rf))

          (`(,acc)
           ;; The alternative is to just skip the second 'keymap
           ;; and handle all binding as if they belonged to the child keymap.
           (funcall rf
                    (if parent-keymap
                        (funcall rf
                                 acc
                                 (reverse parent-keymap))
                      acc)))

          (`(,acc ,x)
           (cond
            (collecting-parent
             (push x parent-keymap)
             acc)

            ((eq x 'keymap)
             (if first-keymap-sym-seen
                 (progn
                   (setf collecting-parent t)
                   (push x parent-keymap)
                   acc)
               (setf first-keymap-sym-seen t)
               (funcall rf acc x)))

            (t
             (funcall rf acc x)))))))))


(ert-deftest test-pathfinder--make-parent-keymap-repack-and-tag-xf ()
  (let ((rf (lambda (&rest args)
              (pcase args
                ('() '())
                (`(,acc) (reverse acc))
                (`(,acc ,x) (cons x acc))))))

    (pcase-dolist (`(,input . ,result)
                   (list
                    ;; No parent keymap
                    (cons '(keymap (?h . self-insert-command))
                          '(keymap (?h . self-insert-command)))

                    ;; Parent keymap present
                    (cons '(keymap (?h . self-insert-command)
                                   (?t . self-insert-command) .
                                   (keymap (?q . self-insert-command)))
                          '(keymap (?h . self-insert-command)
                                   (?t . self-insert-command)
                                   (keymap (?q . self-insert-command))))

                    ;; Parent kaymap present and came from a symbol
                    (let ((map (make-sparse-keymap))
                          (parent (make-sparse-keymap))
                          (sym (gensym)))
                      (keymap-set map "f" 'self-insert-command)
                      (keymap-set parent "g" 'self-insert-command)
                      (setf (symbol-function sym) parent)
                      (set-keymap-parent map sym)
                      (cons map
                            '(keymap (?f . self-insert-command)
                                     (keymap (?g . self-insert-command)))))

                    ;; Other keymaps present in the original map are not affected...
                    (cons '(keymap (keymap) (keymap))
                          '(keymap (keymap) (keymap)))

                    ;; ... even when a parent keymap is present
                    (cons '(keymap (keymap) (keymap) keymap)
                          '(keymap (keymap) (keymap) (keymap)))))

      (should (equal (t-transduce (pathfinder--make-parent-keymap-repack-and-tag-xf)
                                  rf
                                  '()
                                  input)
                     result)))))



;; A little transdusers library.
(defun t-transduce (xf f &rest args)
  (pcase args
    (`(,col)
     (t-transduce xf f (funcall f) col))

    (`(,init ,col)
     (let* ((f (funcall xf f))
            (fold (cl-reduce f col
                             :initial-value init)))
       (funcall f fold)))))


(defun t-compose (f &rest fs)
  (pcase fs
    ('() f)
    (`(,g) (lambda (&rest args)
             (funcall f (pcase args
                          (`() (funcall g))
                          (`(,a) (funcall g a))
                          (`(,a ,b) (funcall g a b))
                          (_ (apply g args))))))
    (`(,g . ,rest) (cl-reduce #'t-compose
                              `(,f ,g ,@rest)))))


(defun t-completing (f &optional cf)
  (let ((cf (or cf #'identity)))
    (lambda (&rest args)
      (pcase args
        ('() (funcall f))
        (`(,a) (funcall cf a))
        (`(,a ,b) (funcall f a b))))))


(defun t-map (f)
  (lambda (rf)
    (lambda (&rest args)
      (pcase args
        ('() (funcall rf))
        (`(,acc) (funcall rf acc)) ;; ???
        (`(,acc ,x) (funcall rf
                             acc
                             (funcall f x)))))))


(defun t-filter (f)
  (lambda (rf)
    (lambda (&rest args)
      (pcase args
        ('() (funcall rf))
        (`(,acc) (funcall rf acc))
        (`(,acc ,x) (if (funcall f x)
                        (funcall rf acc x)
                      acc))))))


;;; Transducers Tests:
(ert-deftest test-t-transduce ()
  ;; Col is empty; init is provided.
  (should (equal (t-transduce #'identity
                              #'+
                              '())
                 0))

  ;; Col is empty; init is not provided.
  (should (equal (t-transduce #'identity
                              #'+
                              5
                              '())
                 5))

  ;; Init is provided.
  (should (equal (t-transduce #'identity
                              #'+
                              0
                              '(1 2 3))
                 6))

  ;; Init is not provided.
  (should (equal (t-transduce #'identity
                              #'+
                              '(1 2 3))
                 6)))


(ert-deftest test-t-compose ()
  (should (equal (t-compose #'+) #'+))
  (let ((f (t-compose (lambda (&rest args)
                        `(outer ,@args))
                      (lambda (&rest args)
                        `(middle ,@args))
                      (lambda (&rest args)
                        `(inner ,@args)))))

    (should (equal (funcall f)
                   '(outer (middle (inner)))))

    (should (equal (funcall f 1)
                   '(outer (middle (inner 1)))))

    (should (equal (funcall f 1 2)
                   '(outer (middle (inner 1 2)))))

    (should (equal (funcall f 1 2 3)
                   '(outer (middle (inner 1 2 3)))))))


(ert-deftest test-t-completing ()
  (should (let ((f (t-completing #'-)))
            (and (equal (funcall f)
                        (-))
                 ;; Completing with no cf provided turns any function with arity-1
                 ;; into the identity function.
                 (equal (funcall f 3)
                        3)
                 (equal (funcall f 2 4)
                        (- 2 4)))))


  (should (let ((f (t-completing #'-
                                 (lambda (x) (* x x)))))
            (and (equal (funcall f)
                        (-))
                 ;; Completing with cf provided turns any function with arity-1
                 ;; into cf.
                 (equal (funcall f -3)
                        9)
                 (equal (funcall f 2 4)
                        (- 2 4))))))


(ert-deftest test-t-map ()
  (should (equal (t-transduce (t-map (lambda (x) (* x x)))
                              #'+
                              '(1 2 3 4))
                 30)))


(ert-deftest test-t-filter ()
  (should (equal (t-transduce (t-filter #'cl-oddp)
                              #'+
                              '(1 2 3 4 5))
                 9))

  (should (equal (t-transduce (t-filter #'cl-oddp)
                              #'+
                              '(1))
                 1)))

;;; END: Transducer tests



(provide 'pathfinder)
;;; pathfinder.el ends here.

;; Local Variables:
;; eval: (flycheck-disable-checker 'emacs-lisp-checkdoc)
;; eval: (smartparens-mode -1)
;; eval: (electric-pair-mode t)
;; End:
