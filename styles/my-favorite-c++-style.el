(defconst my-favorite-c++-style
  `("Metrika"
    (indent-tabs-mode . nil)
    (tab-width . 4)))


(defun my-favorite-set-c++-style ()
  (interactive)
  (c-set-style "Favorite"))


(add-hook 'c-initialization-hook
	  (lambda () (c-add-style "Favorite" my-favorite-c++-style)))

(provide 'my-favorite-c++-style)
