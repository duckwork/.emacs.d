This is my Emacs configuration.  It's also a literate `org-mode` file.  Yeah, I'm a cool guy.


# About me

	;; init.el -*- lexical-binding: t -*-
	(setq user-full-name "Case Duckworth"
		  user-mail-address "acdw@acdw.net")


# License

Copyright © 2020 Case Duckworth <acdw@acdw.net>

This work is free.  You can redistribute it and/or modify it under the terms of the Do What the Fuck You Want To Public License, Version 2, as published by Sam Hocevar.  See the `LICENSE` file, tangled from the following source block, for details.

	DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE

	Version 2, December 2004

	Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>

	Everyone is permitted to copy and distribute verbatim or modified copies of
	this license document, and changing it is allowed as long as the name is changed.

	DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE

	TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

	   0. You just DO WHAT THE FUCK YOU WANT TO.


## Note on the license

It's highly likely that the WTFPL is completely incompatible with the GPL, for what should be fairly obvious reasons.  To that, I say:

**SUE ME, RMS!**


# Bootstrap


## Original init.el

	;; This file replaces itself with the actual configuration when
	;; first run.  To keep only this version in git, run this command:
	;;
	;; git update-index --assume-unchanged init.el
	;;
	;; If it needs to be changed, start tracking it again thusly:
	;;
	;; git update-index --no-assume-unchanged init.el

	(require 'org)
	(find-file (concat user-emacs-directory "config.org"))
	(org-babel-tangle)
	(load-file (concat user-emacs-directory "early-init.el"))
	(load-file (concat user-emacs-directory "init.el"))
	(byte-compile-file (concat user-emacs-directory "init.el"))


## Tangling

	(defun acdw/tangle-init ()
	  "If the current buffer is `config.org', tangle it, then compile
	and load the resulting files."
	  (when (equal (buffer-file-name)
    	       (expand-file-name
    		(concat user-emacs-directory "config.org")))
		(require 'async)
		(async-start
		 (lambda ()
		   (let ((prog-mode-hook nil))
    	 (require 'org)
    	 (org-babel-tangle-file
    	  (expand-file-name
    	   (concat user-emacs-directory "config.org")))))
		 (lambda (response)
		   (acdw/load-init)
		   (message "Tangled and loaded: %s" response)))))

	(add-hook 'after-save-hook #'acdw/tangle-init)

	(defun acdw/load-init ()
	  (interactive)
	  (load-file (expand-file-name
    	      (concat user-emacs-directory "early-init.el")))
	  (load-file (expand-file-name
    	      (concat user-emacs-directory "init.el"))))


## Miscellaneous bootstrappy stuff


### Add `~/.emacs.d/lisp/` to `load-path`

	(add-to-list 'load-path
    	     (concat user-emacs-directory
    		     (convert-standard-filename "lisp/")))


### Require my secrets

	(require 'acdw-secrets)


# Early initiation

	;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-

	(setq load-prefer-newer t)


## Increase the garbage collector

	(setq gc-cons-threshold (* 100 100 1000))

	(add-hook 'after-init-hook
    	  (lambda ()
    	    (setq gc-cons-threshold (* 100 100 100))
    	    (message "gc-cons-threshold restored to %S"
    		     gc-cons-threshold)))


## Add more paths to the `exec-path` when using Windows

	(when (eq system-type 'windows-nt)
	  (dolist (path '("~/bin"
    		  "C:/Users/aduckworth/Downloads/PortableGit/bin"
    		  "C:/Users/aduckworth/Downloads/PortableGit/usr/bin"))
		(add-to-list 'exec-path path)))


## Bootstrap `straight.el`

	(defvar bootstrap-version)
	(let ((bootstrap-file
		   (expand-file-name "straight/repos/straight.el/bootstrap.el"
    			 user-emacs-directory))
		  (bootstrap-version 5))
	  (unless (file-exists-p bootstrap-file)
		(with-current-buffer
    	(url-retrieve-synchronously
    	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
    	 'silent 'inhibit-cookies)
		  (goto-char (point-max))
		  (eval-print-last-sexp)))
	  (load bootstrap-file nil 'nomessage))


## Use `use-package`

	(setq straight-use-package-by-default t)
	(straight-use-package 'use-package)


## Keep `~/.emacs.d` tidy

	(straight-use-package 'no-littering)
	(require 'no-littering)


## Additional `use-package` keywords


### `:custom-update`

	(use-package use-package-custom-update
	  :straight (use-package-custom-update
    	     :host github
    	     :repo "a13/use-package-custom-update"))


## Setup async

	(straight-use-package 'async)
	(require 'async)


# Macros


## Customizing variables

	(defmacro cuss (var val)
	  "Basically `use-package''s `:custom', but without using either."
	  `(progn
		 (funcall (or (get ',var 'custom-set) #'set-default)
    	      ',var ,val)))


# Theme

I'm using the [Modus](https://protesilaos.com/modus-themes/) themes.

	(defmacro modus-themes-format-sexp (sexp &rest objects)
	  `(eval (read (format ,(format "%S" sexp) ,@objects))))

	(dolist (theme '("operandi" "vivendi"))
	  (modus-themes-format-sexp
	   (use-package modus-%1$s-theme
		 :init
		 (setq modus-%1$s-theme-slanted-constructs t
    	   modus-%1$s-theme-bold-constructs t
    	   modus-%1$s-theme-fringes 'subtle
    	   modus-%1$s-theme-mode-line '3d
    	   modus-%1$s-theme-syntax 'yellow-comments
    	   modus-%1$s-theme-intense-hl-line nil
    	   modus-%1$s-theme-intense-paren-match t
    	   modus-%1$s-theme-links nil
    	   modus-%1$s-theme-no-mixed-fonts nil
    	   modus-%1$s-theme-prompts nil
    	   modus-%1$s-theme-completions nil
    	   modus-%1$s-theme-diffs nil
    	   modus-%1$s-theme-org-blocks 'grayscale
    	   modus-%1$s-theme-headings
    	   '((1 . section)
    	     (2 . line)
    	     (t . rainbow-line-no-bold))
    	   modus-%1$s-theme-variable-pitch-headings nil
    	   modus-%1$s-theme-scale-headings t
    	   modus-%1$s-theme-scale-1 1.1
    	   modus-%1$s-theme-scale-2 1.15
    	   modus-%1$s-theme-scale-3 1.21
    	   modus-%1$s-theme-scale-4 1.27
    	   modus-%1$s-theme-scale-5 1.33))
	   theme))

I also want to switch themes between night and day.

	(use-package theme-changer
	  :custom
	  (calendar-latitude 30.39)
	  (calendar-longitude -91.83)
	  :config
	  (change-theme 'modus-operandi 'modus-vivendi))


# Simplify GUI


## Remove unneeded GUI elements

	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(horizontal-scroll-bar-mode -1)


## Word wrap and operate visually

	(global-visual-line-mode 1)


## Modeline

	(use-package smart-mode-line
	  :custom
	  (sml/no-confirm-load-theme t)
	  :config
	  (sml/setup))

	(defun rm/whitelist-add (regexp)
	  "Add a REGEXP to the whitelist for `rich-minority'."
	  (if (listp 'rm--whitelist-regexps)
		  (add-to-list 'rm--whitelist-regexps regexp)
		(setq rm--whitelist-regexps `(,regexp)))
	  (setq rm-whitelist
    	(mapconcat 'identity rm--whitelist-regexps "\\|")))

	(use-package rich-minority
	  :config
	  (rm/whitelist-add "^$"))


## Show `^L` as a line

	(use-package form-feed
	  :hook
	  ((text-mode prog-mode) . form-feed-mode))


## Cursor

	(cuss cursor-type 'bar)
	(cuss cursor-in-non-selected-windows 'hollow)


# Fonts

See [this StackExchange question and answer](https://emacs.stackexchange.com/questions/12351/when-to-call-find-font-if-launching-emacs-in-daemon-mode) for more information on why I have these font settings applied in a hook.

	(require 'cl)
	(defun font-candidate (&rest fonts)
	  (loop for font in fonts
    	when (find-font (font-spec :name font))
    	return font))

	(defun acdw/setup-fonts ()
	  "Setup fonts.  This has to happen after the frame is set up for
	  the first time, so add it to `focus-in-hook'.  It removes
	  itself."
	  (interactive)
	  (set-face-attribute 'default nil
    		      :font
    		      (font-candidate
    		       "Libertinus Mono-11"
    		       "Linux Libertine Mono O-11"
    		       "Go Mono-11"
    		       "Consolas-11"))

	  (set-face-attribute 'fixed-pitch nil
    		      :font
    		      (font-candidate
    		       "Libertinus Mono-11"
    		       "Linux Libertine Mono O-11"
    		       "Go Mono-11"
    		       "Consolas-11"))

	  (set-face-attribute 'variable-pitch nil
    		      :font
    		      (font-candidate
    		       "Libertinus Serif-14"
    		       "Linux Libertine O-12"
    		       "Georgia-11"))

	  (remove-hook 'focus-in-hook #'acdw/setup-fonts))

	(add-hook 'focus-in-hook #'acdw/setup-fonts)


## Unicode

	(use-package unicode-fonts
	  :config
	  (unicode-fonts-setup))


## Variable pitch faces

	(add-hook 'text-mode-hook #'variable-pitch-mode)


# Ease of use


## Selectrum & Prescient

	(use-package selectrum
	  :config
	  (selectrum-mode 1))

	(use-package prescient
	  :config
	  (prescient-persist-mode 1))

	(use-package selectrum-prescient
	  :after (selectrum prescient)
	  :config
	  (selectrum-prescient-mode 1))


## CtrlF

	(use-package ctrlf
	  :custom
	  (ctrlf-show-match-count-at-eol nil)
	  :bind
	  ("C-s" . ctrlf-forward-regexp)
	  ("C-r" . ctrlf-backward-regexp)
	  ("C-M-s" . ctrlf-forward-literal)
	  ("C-M-r" . ctrlf-backward-literal)
	  :config
	  (ctrlf-mode 1))


## Startup

	(cuss inhibit-startup-buffer-menu t)
	(cuss inhibit-startup-screen t)
	(cuss initial-buffer-choice t)
	(cuss initial-scratch-message ";; Hi there!\n")


## Ignore case

	(cuss completion-ignore-case t)
	(cuss read-buffer-completion-ignore-case t)
	(cuss read-file-name-completion-ignore-case t)


## Which key

	(use-package which-key
	  :custom
	  (which-key-popup-type 'minibuffer)
	  :config
	  (which-key-mode))


## Miscellaneous settings


### Set view mode when in a read-only file

	(cuss view-read-only t)


### Don't use dialog boxen

	(cuss use-dialog-box nil)


### Enable all functions

	(cuss disabled-command-function nil)


### Shorter confirmations

	(fset 'yes-or-no-p #'y-or-n-p)


### Uniquify buffer names

	(require 'uniquify)
	(cuss uniquify-buffer-name-style 'forward)


### Show buffer boundaries

	(cuss indicate-buffer-boundaries
		  '((top . right)
    	(bottom . right)
    	(t . nil)))


### Hippie expand

	(global-set-key (kbd "M-/") 'hippie-expand)


### iBuffer

	(global-set-key (kbd "C-x C-b") 'ibuffer)


### Zap-up-to-char, not zap-to-char

	(autoload 'zap-up-to-char "misc"
	  "Kill up to, but not including, ARGth occurrence of CHAR." t)

	(global-set-key (kbd "M-z") 'zap-up-to-char)


### Other "[better defaults](https://git.sr.ht/~technomancy/better-defaults/tree/master/better-defaults.el)"

	(cuss save-interprogram-paste-before-kill t)
	(cuss apropos-do-all t)
	(cuss mouse-yank-at-point t)
	(cuss require-final-newline t)
	(cuss visible-bell (not (string= (system-name) "larry")))
	(cuss ediff-window-setup-function #'ediff-setup-windows-plain)


### So-long-mode

	(if (boundp 'global-so-long-mode)
		(global-so-long-mode))


### Change `just-one-space` to `cycle-space`

	(defun acdw/cycle-spacing-1 ()
	  (interactive)
	  (cycle-spacing -1))

	(bind-key [remap just-one-space] #'acdw/cycle-spacing-1)


# Persistence


## Auto-saves

	(use-package super-save
	  :custom
	  (auto-save-default nil)
	  (super-save-exclue '(".gpg"))
	  :config
	  (super-save-mode 1))


## Backup files

	(cuss backup-directory-alist
		  `((".*" . ,(no-littering-expand-var-file-name "backup/"))))

	(cuss backup-by-copying 1)
	(cuss delete-old-versions -1)
	(cuss version-control t)
	(cuss vc-make-backup-files t)


## Recent files

	(use-package recentf
	  :custom-update
	  (recentf-exclude
	   '(no-littering-var-directory
		 no-littering-etc-directory))
	  :custom
	  (recentf-max-menu-items 100)
	  (recentf-max-saved-items 100)
	  :config
	  (recentf-mode 1))


### Easily navigate recent files

	(defun recentf-find-file ()
	  "Find a recent file using `completing-read'."
	  (interactive)
	  (let ((file (completing-read "Recent file: " recentf-list nil t)))
		(when file
		  (find-file file))))

	(bind-key "C-x C-r" #'recentf-find-file)


## Save places in visited files

	(use-package saveplace
	  :custom
	  (save-place-file (no-littering-expand-var-file-name "places"))
	  (save-place-forget-unreadable-files (not
    				       (eq system-type 'windows-nt))
	  :config
	  (save-place-mode 1)))


## Save history

	(use-package savehist
	  :custom
	  (savehist-additional-variables
	   '(kill-ring
		 search-ring
		 regexp-search-ring))
	  (savehist-save-minibuffer-history t)
	  (history-length t)
	  (history-delete-duplicates t)
	  :config
	  (savehist-mode 1))


## Undo

	(use-package undo-fu-session
	  :after (no-littering undo-fu)
	  :custom
	  (undo-fu-session-incompatible-files
	   '("COMMIT_EDITMSG\\'"
		 "/git-rebase-todo\\'"))
	  (undo-fu-session-directory
	   (no-littering-expand-var-file-name "undos/"))
	  :config
	  (global-undo-fu-session-mode 1))


# General editing


## File encoding

I'm going to be honest &#x2013; most of this is a stab in the dark.

	(set-language-environment 'utf-8)
	(set-terminal-coding-system 'utf-8)
	(cuss locale-coding-system 'utf-8)
	(set-default-coding-systems 'utf-8)
	(set-selection-coding-system 'utf-8)
	(prefer-coding-system 'utf-8)

	;; from https://www.emacswiki.org/emacs/EndOfLineTips

	(defun acdw/no-junk-please-were-unixish ()
	  "Convert line endings to UNIX, dammit."
	  (let ((coding-str (symbol-name buffer-file-coding-system)))
		(when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
		  (set-buffer-file-coding-system 'unix))))

	(add-hook 'find-file-hooks #'acdw/no-junk-please-were-unixish)


## Undo

	(use-package undo-fu
	  :bind
	  ("C-/" . undo-fu-only-undo)
	  ("C-?" . undo-fu-only-redo))


## Find/replace

	(use-package visual-regexp
	  :bind
	  ("C-c r" . 'vr/replace)
	  ("C-c q" . 'vr/query-replace))


## Visual editing


### Volatile highlights

	(use-package volatile-highlights
	  :config
	  (volatile-highlights-mode 1))


### Expand region

	(use-package expand-region
	  :bind
	  ("C-=" . er/expand-region)
	  ("C-+" . er/contract-region))


## Clean up white space on save

	(add-hook 'before-save-hook #'whitespace-cleanup)
	(add-hook 'before-save-hook #'delete-trailing-whitespace)


## Automatically revert a file to what it is on disk

	(global-auto-revert-mode 1)


# Writing


## Word count

	(use-package wc-mode
	  :config
	  (rm/whitelist-add "WC")
	  :hook text-mode)


## Visual fill column mode

	(use-package visual-fill-column
	  :custom
	  (split-window-preferred-function
	   'visual-fill-column-split-window-sensibly)
	  (visual-fill-column-center-text t)
	  (fill-column 80)
	  :config
	  (advice-add 'text-scale-adjust
    	      :after #'visual-fill-column-adjust)
	  :hook
	  (text-mode . visual-fill-column-mode))


## Org mode

	(use-package org
	  :custom
	  (org-startup-indented t)
	  (org-src-tab-acts-natively t)
	  (org-hide-emphasis-markers t)
	  (org-fontify-done-headline t)
	  (org-fontify-whole-heading-line t)
	  (org-hide-leading-stars t)
	  (org-hidden-keywords '(author date title))
	  (org-src-window-setup 'current-window)
	  (org-pretty-entities t))


### Enable markdown export

	(require 'ox-md)


### Ensure blank lines between headings and before contents

from [unpackaged.el](https://github.com/alphapapa/unpackaged.el#ensure-blank-lines-between-headings-and-before-contents)

	;;;###autoload
	(defun unpackaged/org-fix-blank-lines (&optional prefix)
	  "Ensure that blank lines exist between headings and between
	headings and their contents.  With prefix, operate on whole
	buffer.  Ensures that blank lines exist after each headings's
	drawers."
	  (interactive "P")
	  (org-map-entries
	   (lambda ()
		 (org-with-wide-buffer
		  ;; `org-map-entries' narrows the buffer, which prevents us
		  ;; from seeing newlines before the current heading, so we
		  ;; do this part widened.
		  (while (not (looking-back "\n\n" nil))
    	;; Insert blank lines before heading.
    	(insert "\n")))
		 (let ((end (org-entry-end-position)))
		   ;; Insert blank lines before entry content.
		   (forward-line)
		   (while (and (org-at-planning-p)
    		   (< (point) (point-max)))
    	 ;; Skip planning lines
    	 (forward-line))
		   (while (re-search-forward org-drawer-regexp end t)
    	 ;; Skip drawers.  You might think that
    	 ;; `org-at-drawer-p' would suffice, but for some reason
    	 ;; it doesn't work correctly when operating on hidden
    	 ;; text.  This works, taken from
    	 ;; `org-agenda-get-some-entry-text'.
    	 (re-search-forward "^[ \t]*:END:.*\n?" end t)
    	 (goto-char (match-end 0)))
		   (unless (or (= (point) (point-max))
    		   (org-at-heading-p)
    		   (looking-at-p "\n"))
    	 (insert "\n"))))
	   t (if prefix
    	 nil
		   'tree)))


### `org-return-dwim`

from [unpackaged.el](https://github.com/alphapapa/unpackaged.el#org-return-dwim)

	(defun unpackaged/org-element-descendant-of (type element)
	  "Return non-nil if ELEMENT is a descendant of TYPE.
	TYPE should be an element type, like `item' or `paragraph'.
	ELEMENT should be a list like that returned by
	`org-element-context'."
	  ;; MAYBE: Use `org-element-lineage'.
	  (when-let* ((parent (org-element-property :parent element)))
		(or (eq type (car parent))
    	(unpackaged/org-element-descendant-of type parent))))

	;;;###autoload
	(defun unpackaged/org-return-dwim (&optional default)
	  "A helpful replacement for `org-return'.  With prefix, call `org-return'.

	On headings, move point to position after entry content.  In
	lists, insert a new item or end the list, with checkbox if
	appropriate.  In tables, insert a new row or end the table."
	  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
	  (interactive "P")
	  (if default
		  (org-return)
		(cond
		 ;; Act depending on context around point.

		 ;; NOTE: I prefer RET to not follow links, but by uncommenting this block,
		 ;; links will be followed.

		 ;; ((eq 'link (car (org-element-context)))
		 ;;  ;; Link: Open it.
		 ;;  (org-open-at-point-global))

		 ((org-at-heading-p)
		  ;; Heading: Move to position after entry content.
		  ;; NOTE: This is probably the most interesting feature of this function.
		  (let ((heading-start (org-entry-beginning-position)))
    	(goto-char (org-entry-end-position))
    	(cond ((and (org-at-heading-p)
    		    (= heading-start (org-entry-beginning-position)))
    	       ;; Entry ends on its heading; add newline after
    	       (end-of-line)
    	       (insert "\n\n"))
    	      (t
    	       ;; Entry ends after its heading; back up
    	       (forward-line -1)
    	       (end-of-line)
    	       (when (org-at-heading-p)
    		 ;; At the same heading
    		 (forward-line)
    		 (insert "\n")
    		 (forward-line -1))
    	       ;; FIXME: looking-back is supposed to be called with more arguments.
    	       (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
    		 (insert "\n"))
    	       (forward-line -1)))))

		 ((org-at-item-checkbox-p)
		  ;; Checkbox: Insert new item with checkbox.
		  (org-insert-todo-heading nil))

		 ((org-in-item-p)
		  ;; Plain list.  Yes, this gets a little complicated...
		  (let ((context (org-element-context)))
    	(if (or (eq 'plain-list (car context))  ; First item in list
    		(and (eq 'item (car context))
    		     (not (eq (org-element-property :contents-begin context)
    			      (org-element-property :contents-end context))))
    		(unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
    	    ;; Non-empty item: Add new item.
    	    (org-insert-item)
    	  ;; Empty item: Close the list.
    	  ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
    	  (delete-region (line-beginning-position) (line-end-position))
    	  (insert "\n"))))

		 ((when (fboundp 'org-inlinetask-in-task-p)
    	(org-inlinetask-in-task-p))
		  ;; Inline task: Don't insert a new heading.
		  (org-return))

		 ((org-at-table-p)
		  (cond ((save-excursion
    	       (beginning-of-line)
    	       ;; See `org-table-next-field'.
    	       (cl-loop with end = (line-end-position)
    			for cell = (org-element-table-cell-parser)
    			always (equal (org-element-property :contents-begin cell)
    				      (org-element-property :contents-end cell))
    			while (re-search-forward "|" end t)))
    	     ;; Empty row: end the table.
    	     (delete-region (line-beginning-position) (line-end-position))
    	     (org-return))
    	    (t
    	     ;; Non-empty row: call `org-return'.
    	     (org-return))))
		 (t
		  ;; All other cases: call `org-return'.
		  (org-return)))))

	(bind-key "RET" #'unpackaged/org-return-dwim 'org-mode-map)


# Coding


## Formatting


### Indenting

	(use-package aggressive-indent
	  :config
	  (global-aggressive-indent-mode 1))


### Smart tabs

	(use-package smart-tabs-mode
	  :custom
	  (whitespace-style
	   '(face trailing tabs spaces lines newline
    	  empty indentation space-before-tab
    	  space-mark tab-mark newline-mark))
	  :config
	  (smart-tabs-insinuate 'c 'c++ 'javascript 'java 'ruby))


## Display


### Prettify symbols mode

	(add-hook 'prog-mode-hook #'prettify-symbols-mode)


### Parentheses and frens

1.  `show-paren-style`

		(cuss show-paren-style 'mixed)
		(show-paren-mode 1)

2.  Smartparens

		(use-package smartparens
		  :init
		  (defun acdw/setup-smartparens ()
			(require 'smartparens-config)
			(smartparens-mode 1))
		  :hook
		  (prog-mode . acdw/setup-smartparens))

3.  Rainbow delimiters

		(use-package rainbow-delimiters
		  :hook (prog-mode . rainbow-delimiters-mode))


### Rainbow mode

	(use-package rainbow-mode
	  :custom
	  (rainbow-x-colors nil)
	  :hook prog-mode)


### Line numbers

	(defun acdw/enable-line-numbers ()
	  "Enable line numbers, either through `display-line-numbers-mode'
	or through `linum-mode'."
	  (if (and (fboundp 'display-line-numbers-mode)
    	   (display-graphic-p))
		  (progn
    	(display-line-numbers-mode 1)
    	(cuss display-line-numbers-width 2))
		(linum-mode 1)))

	(add-hook 'prog-mode-hook #'acdw/enable-line-numbers)


## Git

	(use-package magit
	  :bind
	  ("C-x g" . magit-status)
	  :custom-update
	  (magit-no-confirm '(stage-all-changes)))


### Hook into `prescient`

	(define-advice magit-list-refs
		(:around (orig &optional namespaces format sortby)
    	     prescient-sort)
	  "Apply prescient sorting when listing refs."
	  (let ((res (funcall orig namespaces format sortby)))
		(if (or sortby
    	    magit-list-refs-sortby
    	    (not selectrum-should-sort-p))
    	res
		  (prescient-sort res))))


### Use `libgit` when I can build it (requires `cmake`)

	(when (executable-find "cmake")
	  (use-package libgit)
	  (use-package magit-libgit))


### Git "forge" capabilities

	(use-package forge
	  :after magit
	  :unless (eq system-type 'windows-nt)
	  :custom
	  (forge-owned-accounts
	   '(("duckwork"))))


## Programming languages


### Fish shell

	(use-package fish-mode)


### Lisps

1.  Common Lisp (SLIME)

		(use-package slime
		  :when (executable-find "sbcl")
		  :custom
		  (inferior-lisp-program "sbcl")
		  (slime-contribs '(slime-fancy)))

2.  Fennel

		(use-package fennel-mode
		  :mode "\\.fnl\\'")


### Lua

	(use-package lua-mode
	  :mode "\\.lua\\'"
	  :interpreter "lua")


### Web (HTML/CSS/JS)

	(use-package web-mode
	  :mode (("\\.ts\\'" . web-mode)
    	 ("\\.html?\\'" . web-mode)
    	 ("\\.css?\\'" . web-mode)
    	 ("\\.js\\'" . web-mode)))


### `~/.ssh/config`

	(use-package ssh-config-mode)


### Go

	(use-package go-mode
	  :mode "\\.go\\'")


# Applications


## Elpher

	(use-package elpher
	  :straight (elpher
    	     :repo "git://thelambdalab.xyz/elpher.git")
	  :custom
	  (elpher-certificate-directory
	   (no-littering-expand-var-file-name "elpher-certificates/"))
	  (elpher-ipv4-always t)
	  :custom-face
	  (elpher-gemini-heading1
	   ((t (:inherit (modus-theme-heading-1)))))
	  (elpher-gemini-heading2
	   ((t (:inherit (modus-theme-heading-2)))))
	  (elpher-gemini-heading3
	   ((t (:inherit (modus-theme-heading-3)))))
	  :config
	  (defun elpher:eww-browse-url (original url &optional new-window)
		"Handle gemini/gopher links with eww."
		(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url)
    	   (require 'elpher)
    	   (elpher-go url))
    	  (t (funcall original url new-window))))
	  (advice-add 'eww-browse-url :around 'elpher:eww-browse-url)
	  :bind (:map elpher-mode-map
    	      ("n" . elpher-next-link)
    	      ("p" . elpher-prev-link)
    	      ("o" . elpher-follow-current-link)
    	      ("G" . elpher-go-current))
	  :hook
	  (elpher-mode . visual-fill-column-mode))

	(use-package gemini-mode
	  :straight (gemini-mode
    	     :repo "https://git.carcosa.net/jmcbray/gemini.el.git")
	  :mode "\\.\\(gemini|gmi\\)\\'"
	  :custom-face
	  (gemini-heading-face-1
	   ((t (:inherit (elpher-gemini-heading1)))))
	  (gemini-heading-face2
	   ((t (:inherit (elpher-gemini-heading2)))))
	  (gemini-heading-face3
	   ((t (:inherit (elpher-gemini-heading3)))))
	  :hook
	  (gemini-mode . visual-fill-column-mode))

	(use-package gemini-write
	  :straight (gemini-write
    	     :repo "https://alexschroeder.ch/cgit/gemini-write")
	  :config
	  (add-to-list 'elpher-gemini-tokens
    	       (acdw-secrets/elpher-gemini-tokens)))

	(use-package post-to-gemlog-blue
	  :straight (post-to-gemlog-blue
    	     :repo "https://git.sr.ht/~acdw/post-to-gemlog-blue.el"))


## Pastebin (0x0)

(use-package 0x0
      :custom
      (0x0-default-service 'ttm))


## EMMS

    (use-package bongo
      :commands 'bongo)
