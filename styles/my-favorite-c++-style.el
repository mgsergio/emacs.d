(defconst my-favorite-c++-style
  `("Metrika"
    (indent-tabs-mode . nil)
    (tab-width . 4)))


(defun my-favorite-set-c++-style ()
  (interactive)
  (add-hook 'c++-mode-hook (lambda ()
			     (c-set-style "Favorite"))))


(c-add-style "Favorite" my-favorite-c++-style)
(provide 'my-favorite-c++-style)
