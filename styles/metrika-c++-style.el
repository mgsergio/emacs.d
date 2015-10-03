


(defconst metrika-c++-style
  `((indent-tabs-mode . t)
    (tab-width . 4)
    (c-basic-offset . 4)
    (c-offsets-alist . ((access-label . -)	; Guessed value
			(block-close . 0)	; Guessed value
			(class-close . 0)	; Guessed value
			(class-open . 0)	; Guessed value
			(defun-block-intro . +)	; Guessed value
			(defun-close . 0)	; Guessed value
			(defun-open . 0)	; Guessed value
			(else-clause . 0)	; Guessed value
			(inclass . +)		; Guessed value
			(inline-close . 0)	; Guessed value
			(member-init-cont . 0)	; Guessed value
			(member-init-intro . +)	; Guessed value
			(statement . 0)		; Guessed value
			(statement-block-intro . +) ; Guessed value
			(stream-op . +)		; Guessed value
			(substatement . +)	; Guessed value
			(substatement-open . 0)	; Guessed value
			(topmost-intro . 0)	; Guessed value
			(annotation-top-cont . 0)
			(annotation-var-cont . +)
			(arglist-close . c-lineup-close-paren)
			(arglist-cont c-lineup-gcc-asm-reg 0)
			(arglist-cont-nonempty . c-lineup-arglist)
			(arglist-intro . c-lineup-arglist-intro-after-paren)
			(block-open . 0)
			(brace-entry-open . 0)
			(brace-list-close . 0)
			(brace-list-entry . 0)
			(brace-list-intro . +)
			(brace-list-open . +)
			(c . c-lineup-C-comments)
			(case-label . 0)
			(catch-clause . 0)
			(comment-intro . c-lineup-comment)
			(composition-close . 0)
			(composition-open . 0)
			(cpp-define-intro c-lineup-cpp-define +)
			(cpp-macro . -1000)
			(cpp-macro-cont . +)
			(do-while-closure . 0)
			(extern-lang-close . 0)
			(extern-lang-open . 0)
			(friend . 0)
			(func-decl-cont . +)
			(incomposition . +)
			(inexpr-class . +)
			(inexpr-statement . +)
			(inextern-lang . +)
			(inher-cont . c-lineup-multi-inher)
			(inher-intro . +)
			(inlambda . c-lineup-inexpr-block)
			(inline-open . 0)
			(inmodule . +)
			(innamespace . 0)  ; TODO Make a function that does not indent single namespace
			(knr-argdecl . 0)
			(knr-argdecl-intro . 5)
			(label . 0)
			(lambda-intro-cont . +)
			(module-close . 0)
			(module-open . 0)
			(namespace-close . 0)
			(namespace-open . 0)
			(objc-method-args-cont . c-lineup-ObjC-method-args)
			(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
			(objc-method-intro . [0])
			(statement-case-intro . +)
			(statement-case-open . +)
			(statement-cont . +)
			(string . -1000)
			(substatement-label . 0)
			(template-args-cont c-lineup-template-args +)
			(topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))
"This style is the default in Yandex.Metrica.")

(defun metrika-set-c++-style ()
  (interactive)
  (c-set-style "Metrika"))

(add-hook 'c-initialization-hook
	  (lambda () (c-add-style "Metrika" metrika-c++-style)))

(provide 'metrika-c++-style)
