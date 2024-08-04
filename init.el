;; Group 
(defgroup matt-e-macs nil
  "User options for my dotemacs.
	    These produce the expected results only when set in a file called
	    matt-e-macs-pre-custom.el.  This file must be in the same
	    directory as the init.el."
  :group 'file)

(defcustom matt-e-macs-load-theme-family 'modus
  "Set of themes to load.
  Valid values are the symbols `ef', `modus', and `standard', which
  reference the `ef-themes', `modus-themes', and `standard-themes',
  respectively.

  A nil value does not load any of the above (use Emacs without a
  theme).

  This user option must be set in the `matt-e-macs-pre-custom.el'
  file.  If that file exists in the Emacs directory, it is loaded
  before all other modules of my setup."
  :group 'matt-e-macs
  :type '(choice :tag "Set of themes to load" :value modus
		 (const :tag "The `ef-themes' module" ef)
		 (const :tag "The `modus-themes' module" modus)
		 (const :tag "The `standard-themes' module" standard)
		 (const :tag "Do not load a theme module" nil)))

(setq custom-safe-themes t)

;; Turn off backups
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;; Make sure to look for .el in matt-e-macs-lisp/ and matt-e-macs-modules/
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("matt-e-macs-lisp" "matt-e-macs-modules"))

(load (locate-user-emacs-file "matt-e-macs-pre-custom.el") :no-error :no-message)
;; preferences BEFORE loading any of the modules.

(require 'matt-e-macs-theme)
;; (require 'matt-e-macs-essentials)
;; (require 'matt-e-macs-modeline)
;; (require 'matt-e-macs-completion)
;; (require 'matt-e-macs-search)
;; (require 'matt-e-macs-dired)
;; (require 'matt-e-macs-window)
;; (require 'matt-e-macs-git)
;; (require 'matt-e-macs-org)
;; (require 'matt-e-macs-langs)
;; (require 'matt-e-macs-email)
;; (require 'matt-e-macs-web)
;; (when matt-e-macs-load-which-key
;;   (require 'matt-e-macs-which-key))
;; (when matt-e-macs-load-icons
;;   (require 'matt-e-macs-icons))

(load (locate-user-emacs-file "matt-e-macs-post-custom.el") :no-error :no-message)
