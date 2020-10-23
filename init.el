;;; init.el -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;; BANKRUPT_SCORE: 1
;; ^ the number of times I've declared Emacs bankruptcy.
;; Does this mean I'm part of the cool kids now?

;;; Macros
(defmacro cuss (var val)
  "Basically `use-package' `:custom' but without either."
  `(progn
     (funcall (or (get ',var 'custom-set) #'set-default)
	      ',var ,val)))

;;; Files
;; keep `emacs-user-directory' tidy
(use-package no-littering)

;; don't clutter `init.el' with customize crap
(cuss custom-file (no-littering-expand-etc-file-name "custom.el"))

;; backups
(cuss backup-directory-alist
      `((".*" . ,(no-littering-expand-var-file-name "backup/"))))

;; recent files
(use-package recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

;; encoding
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Save/Restore
;; autosave, but like, better
(use-package super-save
  :custom
  (auto-save-default nil)
  (super-save-auto-save-when-idle t)
  (super-save-exclude '(".gpg"))
  :config
  (super-save-mode +1))

;; save places in files
(use-package saveplace
  :custom
  (save-place-file (no-littering-expand-var-file-name "places"))
  :config
  (save-place-mode +1))

;; save history of variables
(use-package savehist
  :custom
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring))
  :config
  (savehist-mode +1))

;;; Buffers
;; uniquely name buffers
(cuss uniquify-buffer-name-style 'forward)

;;; UI
;; frame defaults
(cuss default-frame-alist '((tool-bar-lines . 0)
			    (menu-bar-lines . 0)
			    (vertical-scroll-bars . nil)
			    (horizontal-scroll-bars . nil)
			    (right-divider-width . 2)
			    (bottom-divider-width . 2)
			    (left-fringe-width . 2)
			    (right-fringe-width . 2)))

;; also disable these with modes, so I can re-enable them more easily
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; cursor
(cuss cursor-type 'bar)
(cuss cursor-in-non-selected-windows 'hollow)
(blink-cursor-mode 0)

;; mouse
(cuss mouse-yank-at-point t) ; yank at the point instead of where clicked

;; startup screen
(cuss inhibit-startup-buffer-menu t)
(cuss inhibit-startup-screen t)
(cuss initial-buffer-choice t) ; start in *scratch*
(cuss initial-scratch-message nil)

;; interactivity
(fset 'yes-or-no-p #'y-or-n-p)

(cuss use-dialog-box nil)
(cuss disabled-command-function nil)

;; themes
(use-package modus-operandi-theme
  :config
  (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme)

;; fonts
(set-face-attribute 'default nil
		    :family "DejaVu Sans Mono"
		    :height 110)

(set-face-attribute 'fixed-pitch nil
		    :family "DejaVu Sans Mono"
		    :height 110)

(set-face-attribute 'variable-pitch nil
		    :family "DejaVu Serif"
		    :height 120)

;; unicode
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;;; Selecting / Minibuffer
;; ignore case
(cuss completion-ignore-case t)
(cuss read-buffer-completion-ignore-case t)
(cuss read-file-name-completion-ignore-case t)

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :after (selectrum prescient)
  :config
  (selectrum-prescient-mode +1))

;; searching
(use-package ctrlf
  :custom
  (ctrlf-show-match-count-at-eol nil)
  :config
  (ctrlf-mode +1))

;;; Undo
(use-package undo-fu
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-?" . undo-fu-only-redo))

(use-package undo-fu-session
  :after no-littering
  :custom
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'"
     "/git-rebase-todo\\'"))
  (undo-fu-session-directory
   (no-littering-expand-var-file-name "undos/"))
  :config
  (global-undo-fu-session-mode +1))

;;; Text editing
;; visual line mode
(global-visual-line-mode +1)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode +1))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; delete the selection when typing
(delete-selection-mode +1)

;; clipboard
(cuss save-interprogram-paste-before-kill t) ; save existing clipboard text to kill ring before replacing it

;; don't insert tabs.
(cuss indent-tabs-mode nil)

;;; Programming
;; Git
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(when (executable-find "cmake")
  (use-package libgit)
  (use-package magit-libgit))

(use-package forge
  :after magit
  :custom
  (forge-owned-accounts '(("duckwork"))))

;; Code formatting
(use-package format-all
  :hook
  (prog-mode . format-all-mode))

;; display
(add-hook 'prog-mode-hook #'prettify-symbols-mode)

;; parentheses
(cuss show-paren-style 'mixed)
(show-paren-mode +1)

(use-package smartparens
  :init
  (defun acdw/setup-smartparens ()
    (require 'smartparens-config)
    (smartparens-mode +1))
  :hook
  (prog-mode . acdw/setup-smartparens))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; line numbers
(add-hook 'prog-mode-hook
	  (if (and (fboundp 'display-line-numbers-mode)
		   (display-graphic-p))
	      #'display-line-numbers-mode
	    #'linum-mode))

;;; Writing
(cuss sentence-end-double-space t)
