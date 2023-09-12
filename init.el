;;; --- init.el ---
;; It is more modern to use `.emacs.d/init.el`
;;   b/c now you can just git clone your emacs.d
;; This init.el is inspired by: https://github.com/Gavinok/emacs.d/blob/main/init.el

(setq user-full-name "Matthew Shum"
      user-mail-address "matthew.shum@novartis.com")

;;; --- Disable GUI ---
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
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

(use-package emacs
  :ensure nil
  :defer nil
  :bind (("C-c w"   . fixup-whitespace)
         ("C-x C-d" . delete-pair)
         ("M-c"     . capitalize-dwim)
         ("M-u"     . upcase-dwim)
         ("M-l"     . downcase-dwim)
         ("M-z"     . zap-up-to-char)
         ("C-x S"   . shell)
         ("C-x M-t" . transpose-regions)
         ("C-;"     . negative-argument)
         ("C-M-;"   . negative-argument)
         ("M-1" . delete-other-windows)
         ("M-2" . split-window-below)
         ("M-3" . split-window-right))

  :config
  ;; Set the title of the frame to the current file - Emacs
  (setq-default frame-title-format '("%b - Emacs"))

;;;; Defaults
  ;; Handle long lines
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)
  (global-so-long-mode 1)

  (setq-default history-length 1000
                use-dialog-box nil
                delete-by-moving-to-trash t
                create-lockfiles nil
                auto-save-default nil
                inhibit-startup-screen t
                ring-bell-function 'ignore)

;;;; UTF-8
  (prefer-coding-system 'utf-8)
;;;; Remove Extra Ui
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis
  (setq-default x-select-enable-primary t) ; use primary as clipboard in emacs
  ;; avoid leaving a gap between the frame and the screen
  (setq-default frame-resize-pixelwise t)

  ;; Vim like scrolling
  (setq scroll-step            1
        scroll-conservatively  10000
        next-screen-context-lines 5
        ;; move by logical lines rather than visual lines (better for macros)
        line-move-visual nil)

  ;;TRAMP
  (setq tramp-default-method "ssh"
        shell-file-name "bash")         ; don't use zsh

  ;; recentf
  (customize-set-value 'recentf-make-menu-items 150)
  (customize-set-value 'recentf-make-saved-items 150))


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


;;; popup window managment
(use-package popper
  :ensure t ; or :straight t
  :bind (("M-`"     . popper-toggle-latest)
         ("M-~"     . popper-cycle)
         ("C-x M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*xref\\*"
          "\\*Backtrace\\*"
          "*Flymake diagnostics.*"
          "\\*eldoc\\*"
          "\\*compilation\\*"
          "\\*rustic-"
          "^*tex"
          "\\*Ement Notifications\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Dtache Shell Command\\*"
          "\\*mu4e-update\\*"
          "\\*GDB.*out\\*"
          help-mode
          compilation-mode))
  (setq popper-display-control 'user)
  (popper-mode +1))


(use-package autorevert
  :ensure nil
  :defer 1
  :init (global-auto-revert-mode t))


(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))


;; Automatic code formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))


;;; LSP
(use-package lsp-mode
  :defer t
  :bind (("C-h ," . help-at-pt-buffer)
         (:map lsp-mode-map
               ("M-<return>" . lsp-execute-code-action)))
  :commands (lsp lsp-deferred)
  :init
  (setenv "LSP_USE_PLISTS" "1")
  ;; Increase the amount of data emacs reads from processes
  (setq read-process-output-max (* 3 1024 1024))
  ;; ;; Clang
  ;; (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
  ;;                                 "--clang-tidy"
  ;;                                 "--enable-config"))
  
  ;; General lsp-mode settings
  (setq lsp-completion-provider :none
        lsp-enable-snippet t
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-diagnostics-provider :flymake
        lsp-keymap-prefix "C-x L"
        lsp-eldoc-render-all t)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults)
                    '((styles . (orderless-flex))))))
  :config
  (defun help-at-pt-buffer ()
    (interactive)
    (let ((help (help-at-pt-kbd-string))
          (h-at-p-buf "*Help At Point*"))
      (if help
          (progn (with-current-buffer (get-buffer-create h-at-p-buf)
                   (view-mode -1)
                   (erase-buffer) (insert (format "%s" (substitute-command-keys help)))
                   (view-mode +1))
                 (switch-to-buffer-other-window h-at-p-buf))
        (if (not arg) (message "No local help at point")))))
  
  (use-package lsp-ui
    :ensure t
    :after lsp
    :init
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-show-diagnostics t)))

(use-package lsp-languages
  :no-require t :ensure nil
  :hook ((python-mode          . lsp-deferred))
  :init
  ;; (use-package lsp-rust :ensure nil :no-require t
  ;;   :hook (rust-mode       . lsp-deferred)
  ;;   :config
  ;;   (lsp-rust-analyzer-inlay-hints-mode 1))

  (use-package lsp-pyright :ensure t
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred)))
    :init
    (setq python-shell-enable-font-lock nil)
    (setq lsp-use-plists nil)))



;; ;; Rust
;; (use-package rust-mode    :ensure t :mode "\\.rs\\'"
;;   :init
;;   ;; scratchpad for rust
;;   (setq lsp-rust-clippy-preference "on")
;;   (use-package rust-playground
;;     :commands (rust-playground)
;;     :ensure t))


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
 ;; :unless my/is-termux
  :defer 5
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-pre-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode)
  :config (diff-hl-flydiff-mode))


(use-package prog-mode
  :ensure nil
  :hook ((prog-mode       . infer-indentation-style)
         (prog-mode       . (lambda () (setq-local show-trailing-whitespace t)))
         (emacs-lisp-mode . (lambda () (add-hook 'local-write-file-hooks 'check-parens)))
         (lisp-mode       . (lambda () (setq indent-tabs-mode nil)))
         ;; Make all scripts executable. Ya this might be sketch but I don't
         (after-save      . executable-make-buffer-file-executable-if-script-p))
  :bind (:map emacs-lisp-mode-map
              ("C-c RET" . emacs-lisp-macroexpand)
              ("C-c C-k" . eval-buffer))
  :init
  ;; Don't prompt for a reference
  (setq xref-prompt-for-identifier nil)
  (global-prettify-symbols-mode)

  ;; Smart Indentation
  (defun infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t)))))



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

(use-package embark
  :ensure t
  :bind
  ;; pick some comfortable binding
  (("C-="                     . embark-act)
   ("C-<escape>"              . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d"                     . dragon-drop)
   :map embark-defun-map
   ("M-t" . chatgpt-gen-tests-for-region)
   :map embark-general-map
   ("M-c" . chatgpt-prompt)
   :map embark-region-map
   ("?"   . chatgpt-explain-region)
   ("M-f" . chatgpt-fix-region)
   ("M-f" . chatgpt-fix-region))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drop " file))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (:all embark consult)
  :demand t
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; For uploading files
(use-package 0x0
  :ensure t
  :after embark
  :bind (
         :map embark-file-map
         ("U"    . 0x0-upload-file)
         :map embark-region-map
         ("U"    . 0x0-dwim))
  :commands (0x0-dwim 0x0-upload-file))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-nippet-dir "~/.emacs.d/snippets")
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t :after yasnippet)

(use-package yasnippet-capf
  :ensure nil
  :quelpa (yasnippet-capf :fetcher github :repo "elken/yasnippet-capf")
  :after yasnippet
  :hook ((prog-mode . yas-setup-capf)
         (text-mode . yas-setup-capf)
         (lsp-mode  . yas-setup-capf)
         (sly-mode  . yas-setup-capf))
  :bind (("C-c y" . yasnippet-capf)
         ("M-+"   . yas-insert-snippet))
  :config
  (defun yas-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'yasnippet-capf
                      completion-at-point-functions)))
  (push 'yasnippet-capf completion-at-point-functions))
