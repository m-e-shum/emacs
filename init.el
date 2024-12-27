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

(defcustom matt-e-macs-completion-ui 'vertico
  "Choose minibuffer completion UI between `mct' or `vertico'.
  If the value is nil, the default completion user interface is
  used.  On Emacs 30, this is close the experience with `mct'.

  This user option must be set in the `matt-e-macs-pre-custom.el'
  file.  If that file exists in the Emacs directory, it is loaded
  before all other modules of my setup."
  :group 'matt-e-macs
  :type '(choice :tag "Minibuffer user interface"
		     (const :tag "Default user interface" nil)
		     (const :tag "The `mct' module" mct)
		     (const :tag "The `vertico' module" vertico)))

(defcustom matt-e-emacs-completion-extras t
    "When non-nil load extras for minibuffer completion.
These include packages such as `consult' and `embark'."
    :group 'matt-e-macs
    :type 'boolean)

(defcustom matt-e-macs-treesitter-extras t
    "When non-nil load extras for tree-sitter integration
These include packages such as `expreg' and generally anything
that adds functionality on top of what the major mode provides."
    :group 'matt-e-macs
    :type 'boolean)

(defcustom matt-e-macs-load-icons nil
    "When non-nil, enable iconography in various contexts.
This installs and uses the `nerd-icons' package and its variants.
NOTE that you still need to invoke `nerd-icons-install-fonts'
manually to first get the icon files.

This user option must be set in the `matt-e-macs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'matt-e-emacs
  :type 'boolean)

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

;; (prot-lisp is Prot's custom code)
;; Make sure to look for .el in prot-lisp/ and matt-e-macs-modules/
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("prot-lisp" "matt-e-macs-modules"))

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
    (setq package-archive-priorities
          '(("gnu-elpa" . 3)
            ("melpa" . 2)
            ("nongnu" . 1)))

(setq package-install-upgrade-built-in t)

(defmacro prot-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

(prot-emacs-keybind global-map
                    "M-TAB" #'indent-relative) ;;

(load (locate-user-emacs-file "matt-e-macs-pre-custom.el") :no-error :no-message)
;; preferences BEFORE loading any of the modules.

(require 'matt-e-macs-theme)
(require 'matt-e-macs-essentials)
(require 'matt-e-macs-modeline)
(require 'matt-e-macs-completion)
(require 'matt-e-macs-treesitter)
;; (require 'matt-e-macs-search)
(require 'matt-e-macs-dired)
;;(require 'matt-e-macs-window)
(require 'matt-e-macs-git)
(require 'matt-e-macs-my-org)
;;  (require 'matt-e-macs-org)
(require 'matt-e-macs-lang)
;; (require 'matt-e-macs-email)
;; (require 'matt-e-macs-web)
;; (when matt-e-macs-load-which-key
;;   (require 'matt-e-macs-which-key))
(when matt-e-macs-load-icons
  (require 'matt-e-macs-icons))

(load (locate-user-emacs-file "matt-e-macs-post-custom.el") :no-error :no-message)
