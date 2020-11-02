

# Preamble

I wanted to write my Emacs configuration in [Org mode](https://orgmode.org) for a while, but never could quite figure out how.  Finally, I found [Lars Tveito](https://github.com/larstvei/dot-emacs)'s config, which does exactly what I want: `init.el` is small and simple, and replaced after the first run, and `init.org` is automatically tangled.  So I'm very excited.


# License

WTFPL.  For more info, see `LICENSE`.

Probably that's not legal under the terms of the GPL or whatever Emacs is licensed under.
SUE ME, RMS


# Bootstrap

*Check out Lars's config for the reasoning behind this.*

When this configuration is loaded for the first time, this `init.el` is loaded:

    ;; This file replaces itself with the actual configuration when first run.  To keep only this version in git, run this command:
    ;; git update-index --assume-unchanged init.el
    ;;
    ;; If it needs to be changed, start tracking it again thusly:
    ;; git update-index --no-assume-unchanged init.el
    
    (require 'org)
    (find-file (concat user-emacs-directory "init.org"))
    (org-babel-tangle)
    (load-file (concat user-emacs-directory "early-init.el"))
    (load-file (concat user-emacs-directory "init.el"))
    (byte-compile-file (concat user-emacs-directory "init.el"))


## Tangling

After the first run, the above `init.el` will be replaced by the tangled stuff here.  However, when *this* file is edited, we'll need to re-tangle everything.  However, nobody has time to do that manually with `C-c C-v t`, *every time*!  Luckily, Emacs is highly programmable.

    (defun acdw/tangle-init ()
      "If the current buffer is `init.org', the code blocks are tangled,
      and the tangled file is compiled and loaded."
      (interactive)
      (when (equal (buffer-file-name)
                   (expand-file-name (concat user-emacs-directory "config.org")))
        ;; Avoid running hooks when tangling.
        (let ((prog-mode-hook nil))
          (org-babel-tangle))))
    
    (add-hook 'after-save-hook #'acdw/tangle-init)


# Early initiation

Emacs 27.1+ uses `early-init.el`, which is evaluated before things like `package.el` and other stuff.  So I have a few settings in there.


## Preamble

Of course, first thing is the modeline.  After that, I set `load-prefer-newer` because, well, it *should*.

      ;;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-
    
    (setq load-prefer-newer t)


## Computers

I have to set these constants before bootstrapping the package manager, since `straight.el` depends on Git, and at work, those are in a weird place.

    (defconst *acdw/at-work* (eq system-type 'windows-nt))
    (defconst *acdw/at-larry* (string= (system-name) "larry"))
    (defconst *acdw/at-bax* (string= (system-name) "bax"))
    (defconst *acdw/at-home* (or *acdw/at-larry* *acdw/at-bax*))


## Package management

I've started using straight.el, which is great.  It grabs packages from git, and apparently will let me fork and edit them, which I'll probably get around to &#x2026; eventually.


### At work, Git's in a weird place

    (when *acdw/at-work*
      (add-to-list 'exec-path "~/bin")
      (add-to-list 'exec-path "C:/Users/aduckworth/Downloads/PortableGit/bin"))


### [straight.el](https://github.com/raxod502/straight.el)

I don't know why, but for some reason the bootstrapping doesn't work on Windows.  I have to download the repo directly from github and put it in the right place (`~/.emacs.d/straight/repos/straight.el/`).

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


### [use-package](https://github.com/jwiegley/use-package)

Yeah, you know it, I know it, we all love it.  It's use-package.

    (setq straight-use-package-by-default t)
    (straight-use-package 'use-package)


# Begin init.el

    ;;; init.el -*- lexical-binding: t; coding: utf-8 -*-
    <<tangle-on-save>>


# Macros


## cuss

I like `use-package`,  but I don't like doing the weird "pseudo-package" stuff a lot of people do in their emacs configs.  Partially because I have to set `:straight nil` on a lot of built-in packages, but also because I think being *that* obsessive over one interface through the whole config is &#x2026; I don't know, short-sighted?

Either way, I *do* like the `:custom` interface that `use-package` has, so I've re-implemented it in my own macro.  This way I don't have to worry about whether to `setq` or `custom-set-variable` or whatever.  Just `cuss`!

    (defmacro cuss (var val)
      "Basically `use-package''s `:custom', but without either."
      `(progn
         (funcall (or (get ',var 'custom-set) #'set-default)
                  ',var ,val)))


# Files


## [Keep .emacs.d tidy](https://github.com/emacscollective/no-littering)

    (straight-use-package 'no-littering)
    (require 'no-littering)


## Customize

I don't like the customize interface, but I still sometimes use it when I'm not sure what the name of a variable is.  So I save the stuff to a file, I just don't load it or keep track of it.

    (cuss custom-file (no-littering-expand-etc-file-name "custom.el"))


## Encoding

    (prefer-coding-system 'utf-8-unix)
    (set-default-coding-systems 'utf-8-unix)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)
    (set-selection-coding-system 'utf-8-unix)
    (set-file-name-coding-system 'utf-8-unix)
    (set-clipboard-coding-system 'utf-8-unix)
    (set-buffer-file-coding-system 'utf-8-unix)
    (cuss locale-coding-system 'utf-8)
    (cuss x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


## Recent files

    (use-package recentf
      :config
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      :custom
      (recentf-max-menu-items 100)
      (recentf-max-saved-items 100)
      :config
      (recentf-mode 1))


## Backups

    (cuss backup-directory-alist
          `((".*" . ,(no-littering-expand-var-file-name "backup/"))))


## [Autosave](https://github.com/bbatsov/super-save)

    (use-package super-save
      :custom
      (auto-save-default nil)
      (super-save-exclude '(".gpg"))
      :config
      (super-save-mode 1))


## [Save places](https://www.emacswiki.org/emacs/SavePlace)

    (use-package saveplace
      :custom
      (save-place-file (no-littering-expand-var-file-name "places"))
      (save-place-forget-unreadable-files (not *acdw/at-work*))
      :config
      (save-place-mode 1))


## [Save history](https://www.emacswiki.org/emacs/SaveHist)

    (use-package savehist
      :custom
      (savehist-addtional-variables
       '(kill-ring
         search-ring
         regexp-search-ring))
      (savehist-save-minibuffer-history t)
      :config
      (savehist-mode 1))


# User interface


## Look


### Frames and windows

1.  Frame defaults

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

2.  Resizing

        (cuss frame-resize-pixelwise t)
        (cuss window-combination-resize t)


### Buffers

    (cuss uniquify-buffer-name-style 'forward)
    
    (cuss indicate-buffer-boundaries
          '((top . right)
            (bottom . right)
            (t . nil)))

1.  Startup buffer

        (cuss inhibit-startup-buffer-menu t)
        (cuss inhibit-startup-screen t)
        (cuss initial-buffer-choice t) ; start in *scratch*
        (cuss initial-scratch-message nil)


### Cursor

    (cuss cursor-type 'bar)
    (cuss cursor-in-non-selected-windows 'hollow)
    (blink-cursor-mode 0)


### Interactivity

1.  Mouse

        (cuss mouse-yank-at-point t)

2.  Dialogs

        (cuss use-dialog-box nil)

3.  Disabled functions

        (cuss disabled-command-function nil)

4.  Function aliases

        (fset 'yes-or-no-p #'y-or-n-p)


### Miscellaneous

1.  Convert `^L` to a line

        (use-package page-break-lines
          :config
          (global-page-break-lines-mode 1))


## Themes: [Modus](https://github.com/protesilaos/modus-themes)

    (use-package modus-operandi-theme)
    (use-package modus-vivendi-theme)


### [Change themes](https://github.com/hadronzoo/theme-changer) based on time of day

    (use-package theme-changer
      :init
      (setq calendar-location-name "Baton Rouge, LA"
            calendar-latitude 30.39
            calendar-longitude -91.83)
      :config
      (change-theme 'modus-operandi 'modus-vivendi))


### Disable the current theme when a theme is interactively loaded

This doesn't happen often, but I'll be ready when it does.

    (defadvice load-theme
        (before disable-before-load (theme &optional no-confirm no-enable) activate)
      (mapc 'disable-theme custom-enabled-themes))


## Modeline: [smart-mode-line](https://github.com/Malabarba/smart-mode-line)

    (use-package smart-mode-line
      :config
      (sml/setup))

I hide all minor-modes by default for a clean modeline.  However, I can add them back by adding them to the whitelist with `(add-to-list 'rm-whitelist " REGEX")`.

    (defun rm-whitelist-add (regexp)
      "Add a regexp to the whitelist."
      (setq rm-whitelist
            (mapconcat 'identity rm--whitelist-regexps "\\|")))
    
    (setq rm--whitelist-regexps '("^$"))
    
    (use-package rich-minority
      :custom
      (rm-whitelist
       (mapconcat 'identity rm--whitelist-regexps "\\|")))


## Fonts

I'm sure there's a better way to do this, but for now, this is the best I've got.  I append to the `face-font-family-alternatives` because I don't know what kind of weird magic they're doing in there.

    (cuss face-font-family-alternatives
          '(("Monospace" "courier" "fixed")
            ("Monospace Serif" "Courier 10 Pitch" "Consolas" "Courier Std" "FreeMono" "Nimbus Mono L" "courier" "fixed")
            ("courier" "CMU Typewriter Text" "fixed")
            ("Sans Serif" "helv" "helvetica" "arial" "fixed")
            ("helv" "helvetica" "arial" "fixed")
            ;; now mine
            ("FixedPitch" "Go Mono" "DejaVu Sans Mono" "Consolas" "fixed")
            ("VariablePitch" "Go" "DejaVu Sans" "Georgia" "fixed")))
    
    (set-face-attribute 'default nil
                        :family "FixedPitch"
                        :height 110)
    
    (set-face-attribute 'fixed-pitch nil
                        :family "FixedPitch"
                        :height 110)
    
    (set-face-attribute 'variable-pitch nil
                        :family "VariablePitch"
                        :height 130)


### Ligatures

These cause big problems with cc-mode (as in, totally freezing everything), so I'm going to comment it out.

    ;; (use-package ligature
    ;;   :straight (ligature
    ;;              :host github
    ;;              :repo "mickeynp/ligature.el")
    ;;   :config
    ;;   (ligature-set-ligatures 'prog-mode
    ;;                           '("++" "--" "/=" "&&" "||" "||="
    ;;                             "->" "=>" "::" "__"
    ;;                             "==" "===" "!=" "=/=" "!=="
    ;;                             "<=" ">=" "<=>"
    ;;                             "/*" "*/" "//" "///"
    ;;                             "\\n" "\\\\"
    ;;                             "<<" "<<<" "<<=" ">>" ">>>" ">>="
    ;;                             "|=" "^="
    ;;                             "**" "--" "---" "----" "-----"
    ;;                             "==" "===" "====" "====="
    ;;                             "</" "<!--" "</>" "-->" "/>"
    ;;                             ":=" "..." ":>" ":<" ">:" "<:"
    ;;                             "::=" ;; add others here
    ;;                             ))
    ;;   :config
    ;;   (global-ligature-mode))


### [Unicode fonts](https://github.com/rolandwalker/unicode-fonts)

    (use-package persistent-soft)
    
    (use-package unicode-fonts
      :after persistent-soft
      :config
      (unicode-fonts-setup))


# Editing


## Completion

I was using company, but I think it might've been causing issues with `awk-mode`, so I'm trying `hippie-mode` right now.  So far, I'm also enjoying not having a popup all the time.

    (bind-key "M-/" #'hippie-expand)


## Ignore case

    (cuss completion-ignore-case t)
    (cuss read-buffer-completion-ignore-case t)
    (cuss read-file-name-completion-ignore-case t)


## Selection & Minibuffer


### Selectrum & Prescient

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


## Search


### CtrlF for searching

    (use-package ctrlf
      :custom
      (ctrlf-show-match-count-at-eol nil)
      :config
      (ctrlf-mode +1)
      :bind
      ("C-s" . ctrlf-forward-regexp))


### [Visual Regexp](https://github.com/benma/visual-regexp.el)

    (use-package visual-regexp
      :bind
      ([remap query-replace] . 'vr/query-replace))


## Undo

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


## Visual editing


### `zap-to-char` replacement

    (use-package zop-to-char
      :bind
      ([remap zap-to-char] . zop-to-char)
      ([remap zap-up-to-char] . zop-up-to-char))


### Operate on a line if there's no current region

    (use-package whole-line-or-region
      :config
      (whole-line-or-region-global-mode +1))


### Expand-region

    (use-package expand-region
      :bind
      ("C-=" . er/expand-region)
      ("C-+" . er/contract-region))


### Volatile highlights

    (use-package volatile-highlights
      :config
      (volatile-highlights-mode 1))


### Visual line mode

    (global-visual-line-mode 1)


### A better `move-beginning-of-line`

    (defun my/smarter-move-beginning-of-line (arg)
      "Move point back to indentation of beginning of line.
    
    Move point to the first non-whitespace character on this line.
    If point is already there, move to the beginning of the line.
    Effectively toggle between the first non-whitespace character and
    the beginning of the line.
    
    If ARG is not nil or 1, move forward ARG - 1 lines first.  If
    point reaches the beginning or end of the buffer, stop there."
      (interactive "^p")
      (setq arg (or arg 1))
    
      ;; Move lines first
      (when (/= arg 1)
        (let ((line-move-visual nil))
          (forward-line (1- arg))))
    
      (let ((orig-point (point)))
        (back-to-indentation)
        (when (= orig-point (point))
          (move-beginning-of-line 1))))
    
    (bind-key "C-a" #'my/smarter-move-beginning-of-line)


## Delete the selection when typing

    (delete-selection-mode 1)


## Clipboard

    (cuss save-interprogram-paste-before-kill t)


## Tabs & Spaces

    (cuss indent-tabs-mode nil)
    (cuss sentence-end-double-space t)


# Programming


## Git

    (use-package magit
      :bind
      ("C-x g" . magit-status)
      :config
      (add-to-list 'magit-no-confirm 'stage-all-changes))
    
    ;; hook into `prescient'
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
    
    (when (executable-find "cmake")
      (use-package libgit)
      (use-package magit-libgit))
    
    (use-package forge
      :after magit
      :custom
      (forge-owned-accounts '(("duckwork"))))


## Code display

    (add-hook 'prog-mode-hook #'prettify-symbols-mode)


### Parentheses

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


## Line numbers

    (add-hook 'prog-mode-hook
              (if (and (fboundp 'display-line-numbers-mode)
                       (display-graphic-p))
                  #'display-line-numbers-mode
                #'linum-mode))


## Languages


### Shell

    (use-package shfmt
      :custom
      (shfmt-arguments '("-i" "4" "-ci"))
      :hook
      (sh-mode . shfmt-on-save-mode))
    
    ;; fish
    (use-package fish-mode)


### Lua

    (use-package lua-mode
      :mode "\\.lua\\'"
      :interpreter "lua")


### Fennel

    (use-package fennel-mode
      :mode "\\.fnl\\'")


### Web

    (use-package web-mode
      :custom
      (web-mode-markup-indent-offset 2)
      (web-mode-code-indent-offset 2)
      (web-mode-css-indent-offset 2)
      :mode (("\\.ts\\'" . web-mode)
             ("\\.html?\\'" . web-mode)
             ("\\.css?\\'" . web-mode)
             ("\\.js\\'" . web-mode)))


### SSH config

    (use-package ssh-config-mode)


# Writing


## Word count

    (use-package wc-mode
      :init
      (rm-whitelist-add "WC")
      :hook
      (text-mode . wc-mode))


## Visual fill column

    (use-package visual-fill-column
      :custom
      (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
      (visual-fill-column-center-text t)
      (fill-column 100)
      :config
      (advice-add 'text-scale-adjust
                  :after #'visual-fill-column-adjust)
      :hook
      (org-mode . visual-fill-column-mode))


## Mixed-pitch

    (use-package mixed-pitch
      :hook
      (text-mode . mixed-pitch-mode))


## Org mode

    (use-package org
      :custom
      (org-startup-indented t)
      (org-src-tab-acts-natively t)
      (org-hide-emphasis-markers t)
      (org-fontify-done-headline t)
      (org-hide-leading-stars t)
      (org-pretty-entities t)
      (org-src-window-setup 'current-window))
    
    (use-package org-superstar
      :hook
      (org-mode . org-superstar-mode))


# Applications


## Gemini & Gopher

    (use-package elpher
      :straight (elpher
                 :repo "git://thelambdalab.xyz/elpher.git")
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
      :mode "\\.\\(gemini|gmi\\)\\'")
    
    (use-package gemini-write
      :straight (gemini-write
                 :repo "https://alexschroeder.ch/cgit/gemini-write"))
    
    (use-package post-to-gemlog-blue
      :straight (post-to-gemlog-blue
                 :repo "https://git.sr.ht/~acdw/post-to-gemlog-blue.el"))


## Pastebin

    (use-package 0x0
      :custom
      (0x0-default-service 'ttm))


## Gnus

    (cuss gnus-select-method
          '(nnimap "imap.fastmail.com"
                   (nnimap-inbox "INBOX")
                   (nnimap-split-methods default)
                   (nnimap-expunge t)
                   (nnimap-stream ssl)))
    
    (cuss gnus-secondary-select-methods
          '((nntp "news.gwene.org")))


## Nov.el: read Ebooks

    (use-package nov
      :mode ("\\.epub\\'" . nov-mode)
      :custom
      (nov-text-width t)
      :hook
      (nov-mode . visual-line-mode)
      (nov-mode . visual-fill-column-mode))


# Machine-specific configurations

    (cond
     (*acdw/at-home*
      (use-package su
        :config
        (su-mode 1))
      (use-package trashed
        :custom
        (delete-by-moving-to-trash t))
      (use-package exec-path-from-shell
        :demand
        :config
        (exec-path-from-shell-initialize)))
     )

