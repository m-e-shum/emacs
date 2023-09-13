;;; --- init.el ---
;; It is more modern to use `.emacs.d/init.el`
;;   b/c now you can just git clone your emacs.d
;; This init.el is inspired by: https://github.com/Gavinok/emacs.d/blob/main/init.el
;;   and https://www.adventuresinwhy.com/post/eglot/

(setq user-full-name "Matthew Shum"
      user-mail-address "matthew.shum@novartis.com")

;;; --- Disable GUI ---
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;;(tool-bar-mode -1)
(menu-bar-mode -1)
;;(scroll-bar-mode -1) ;; Function is void

;;; --- STARTUP ---
;; Minimize garbage collection
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)
;; Lower threshold to speed up garbage collection
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old)
             (setq gc-cons-threshold 20000000))
          t)

;;; --- BACKUPS ---
;; Info about this: https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t)


;;; --- EDITOR settings ---
(global-hl-line-mode t)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

(use-package crux
    :bind (("C-a" . crux-move-beginning-of-line)))

;;; --- PACKAGE LIST ---
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;;; --- BOOSTRAP use-package
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t
      comp-async-report-warnings-errors nil
      comp-deferred-compilation t)

;; Install and load 'quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; Keep custom-set-variables and friends out of my init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; THEME
(load-theme 'dracula t)

;;; --- ASYNC ---
;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1))


(use-package savehist
  :defer 2
  :init (savehist-mode t))

(use-package repeat
  :defer 10
  :init
  (repeat-mode +1))


;;; --- DEFAULTS ---
(use-package undo-fu
  :ensure t
  :bind (("C-x u"   . undo-fu-only-undo)
         ("C-/"     . undo-fu-only-undo)
         ("C-z"     . undo-fu-only-undo)
         ("C-S-z"   . undo-fu-only-redo)
         ("C-x C-u" . undo-fu-only-redo)
         ("C-?"     . undo-fu-only-redo)))
(use-package undo-fu-session ; Persistant undo history
  :ensure t
  :demand t
  :config (global-undo-fu-session-mode))


;;; Aligning Text
(use-package align
  :ensure nil
  :defer t
  :bind ("C-x a a" . align-regexp)
  :config
  ;; Align using spaces
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

;;; BUFFER MANAGMENT
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  (setq ibuffer-saved-filter-groups
        '(("home"
           ("Windows" (and (mode . exwm-mode)
                           (not (name . "qutebrowser"))))
;;           ("Qutebrowser" (name . "qutebrowser"))
           ("Shells" (mode . shell-mode))
           ("emacs-config" (or (filename . ".emacs.d")
                               (filename . "emacs-config")))

           ("Web Dev" (or (mode . html-mode)
                          (mode . css-mode)))
           ("Magit" (name . "\*magit"))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))
           ("Browser" (mode . eaf-mode))
           ("Ement" (name . "\*Ement *"))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "home"))))

;;; ISEARCH
(use-package isearch
  :ensure nil
  :bind (("C-s"     . isearch-forward)
         ("M-R"     . isearch-query-replace)
         ("C-r"     . isearch-backward)
         (:map isearch-mode-map
               ("M-w" . isearch-save-and-exit)
               ("M-R" . isearch-query-replace)
               ("M-/" . isearch-complete))
         ;; (:repeat-map isearch-repeat-map
         ;; ("s" . isearch-repeat-forward))
         )
  :custom ((isearch-lazy-count t)
           (lazy-count-prefix-format nil)
           (lazy-count-suffix-format " [%s of %s]")
           (search-whitespace-regexp ".*?")
           (isearch-lazy-highlight t)
           (isearch-lax-whitespace t)
           (isearch-regexp-lax-whitespace nil))
  :config
  (defun isearch-save-and-exit ()
    "Exit search normally. and save the `search-string' on kill-ring."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (kill-new isearch-string))

  ;; Place cursor at the start of the match similar to vim's t
  ;; C-g will return the cursor to it's orignal position
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end))))


(use-package ffap
  :ensure nil
  :bind ("C-x f" . ffap)
  :custom (find-file-visit-truename t)
  :init
  ;; Save my spot when I jump to another file
  (advice-add 'ffap :before #'push-mark))


(use-package highlight-indent-guides
  ;; provides column highlighting.  Useful when you start seeing too many nested
  ;; layers.
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom-face
  (highlight-indent-guides-stack-character-face ((t  :foreground "#FFF")))
  (highlight-indent-guides-character-face       ((t  :foreground "#EEE")))
  (highlight-indent-guides-top-character-face   ((t  :foreground "#CCC")))
  (highlight-indent-guides-even-face            ((t  :foreground "#BBB")))
  (highlight-indent-guides-odd-face             ((t  :foreground "#AAA")))
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

;; Fix path
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; Git
(use-package magit
  :bind (("C-x v SPC" . magit-status)
         :map project-prefix-map
         ("m" . project-magit))
  :commands (magit project-magit)
  :config
  (add-to-list 'project-switch-commands
               '(project-magit "Magit" m))
  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir))))

(use-package forge :ensure t :after magit)

(use-package ediff
  :after (magit vc)
  :commands (ediff)
  :init
  ;; multiframe just doesn't make sense to me
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :defer 5
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-pre-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode)
  :config (diff-hl-flydiff-mode))




(defcustom project-root-markers
  '("Cargo.toml" ".python-version" ".git" )
       ;; added file identifier above
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
	(throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (let ((path (expand-file-name path)))
    (catch 'found
      (while (not (equal "/" path))
        (if (not (project-root-p path))
            (setq path (file-name-directory (directory-file-name path)))
          (throw 'found (cons 'transient path)))))))

(add-hook 'project-find-functions #'project-find-root)

;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;; eglot
;;; use with python-lsp-server
(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . (lambda () (set-fill-column 88))))
  :config
  ;;
  (add-to-list 'eglot-server-programs
	       '(markdown-mode . ("marksman"))
	       )
  (add-hook 'markdown-mode-hook #'eglot-ensure))


;; Provide drop-down completion.
(use-package company
  :after eglot
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 2)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)
	   )
    :hook (eglot-managed-mode . company-mode)
    )


(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(use-package company-box
  :hook (company-mode . company-box-mode))



;;; COMPLETION
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))
  (load (concat user-emacs-directory
                "lisp/affe-config.el"))
  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  (vertico-mode t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;; Extra Completion Functions
(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
         ("M-y"         . consult-yank-pop)
         ("M-g g"       . consult-goto-line)
         ("M-g M-g"     . consult-goto-line)
         ("M-g f"       . consult-flymake)
         ("M-g i"       . consult-imenu)
         ("M-s l"       . consult-line)
         ("M-s L"       . consult-line-multi)
         ("M-s u"       . consult-focus-lines)
         ("M-s g"       . consult-ripgrep)
         ("M-s M-g"     . consult-ripgrep)
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         ("C-c m"       . my/notegrep)
         :map help-map
         ("a" . consult-apropos)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  ;; :config
  ;; (defun my/notegrep ()
  ;;   "Use interactive grepping to search my notes"
  ;;   (interactive)
  ;;   (consult-ripgrep org-directory))
  (recentf-mode t))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-j" . consult-dir)
         ;; :map minibuffer-local-completion-map
         :map vertico-map
         ("C-x C-j" . consult-dir)))

(use-package consult-recoll
  :bind (("M-s r" . counsel-recoll)
         ("C-c I" . recoll-index))
  :init
  (setq consult-recoll-inline-snippets t)
  :config
  (defun recoll-index (&optional arg) (interactive)
    (start-process-shell-command "recollindex"
                                 "*recoll-index-process*"
                                 "recollindex")))


(use-package flymake
  :defer 10
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . flymake-goto-next-error)
         ("M-g M-p" . flymake-goto-prev-error)
         :repeat-map flymake-repeatmap
         ("p" . flymake-goto-prev-error)
         ("n" . flymake-goto-next-error)
         :map flymake-diagnostics-buffer-mode-map
         ("?" . flymake-show-diagnostic-here)
         :map flymake-project-diagnostics-mode-map
         ("?" . flymake-show-diagnostic-here))
  :hook (prog-mode . (lambda () (flymake-mode t)))
  :config
  (defun flymake-show-diagnostic-here (pos &optional other-window)
    "Show the full diagnostic of this error.
Used to see multiline flymake errors"
    (interactive (list (point) t))
    (let* ((id (or (tabulated-list-get-id pos)
                   (user-error "Nothing at point")))
           (text (flymake-diagnostic-text (plist-get id :diagnostic))))
      (message text)))
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))
