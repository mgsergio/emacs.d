(unless (package-installed-p 'use-packagae)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
