(require 'google-c-style)

(defconst mapsme-c-style
  `("Google"
    (c-offsets-alist . ((access-label . -)
                        (member-init-intro . +)))))

(defun mapsme-set-c-style ()
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-set-style "Mapsme"))

(add-hook 'c-initialization-hook
	  (lambda () (c-add-style "Mapsme" mapsme-c-style)))

(provide 'mapsme-c-style)
