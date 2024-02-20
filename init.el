(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
