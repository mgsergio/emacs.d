(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setf use-package-always-ensure t)

(org-babel-load-file "~/.emacs.d/config.org")
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; Load local init if exists.
;; Local init is a file with configuration specific to a particular
;; environment that should not be a part of a general config.
(let ((local-init (file-name-concat user-emacs-directory
                                    "local-init.el")))
  (when (file-regular-p local-init)
    (load-file local-init)))
