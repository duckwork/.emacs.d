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

;;; Save/Restore
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

;;; Text editing
;; visual line mode
(global-visual-line-mode +1)

;; delete the selection when typing
(delete-selection-mode +1)

;; clipboard
(cuss save-interprogram-paste-before-kill t) ; save existing clipboard text to kill ring before replacing it

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
