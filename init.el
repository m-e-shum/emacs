(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))
;;; use-package
(package-initialize)

(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-verbose t
      comp-async-report-warnings-errors nil
      comp-deferred-compilation t)

;;; quelpa-use-package
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
