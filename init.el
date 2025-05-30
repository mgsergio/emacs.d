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
