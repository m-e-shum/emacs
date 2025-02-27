;;; post-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(load-theme 'modus-vivendi-deuteranopia t)

(use-package emacs
  :ensure nil
  :demand t
  :config
  ;;; General settings
  (setq blink-matching-paren nil)
  (setq help-window-select t)
  :bind
  ( :map global-map
    ("M-c" . capitalize-dwim)
    ("M-l" . downcase-dwim)
    ("M-u" . upcase-dwim)))
