
# Table of Contents

1.  [Preamble](#org0bff216)
2.  [License](#orged58376)
3.  [Bootstrap](#org4c40f13)
    1.  [Tangling](#orgdd48fb7)
4.  [Early initiation](#orgf06a67b)
    1.  [Preamble](#org790f346)
    2.  [Computers](#org27a9d06)
    3.  [Package management](#org982c51d)
        1.  [At work, Git's in a weird place](#org1d43543)
        2.  [straight.el](#orgc3c01af)
        3.  [use-package](#org75b502f)
5.  [Begin init.el](#orge258966)
6.  [Macros](#org429e2c9)
    1.  [cuss](#org98b79c1)
7.  [Files](#orgef614d8)
    1.  [Keep .emacs.d tidy](#orgcd98663)
    2.  [Customize](#orgfa8ddd8)
    3.  [Encoding](#org7d9d257)
    4.  [Recent files](#org5a20cd6)
    5.  [Backups](#org48971aa)
    6.  [Autosave](#org3243bbe)
    7.  [Save places](#org1ad3687)
    8.  [Save history](#orge49f590)
8.  [User interface](#orgbaf79a4)
    1.  [Look](#org57b0df2)
        1.  [Frames and windows](#org7eea925)
        2.  [Buffers](#orgb767f7c)
        3.  [Cursor](#org8983905)
        4.  [Interactivity](#org62c9668)
        5.  [Miscellaneous](#org1ea822c)
    2.  [Themes: Modus](#org677f380)
        1.  [Change themes based on time of day](#orgb821d6f)
        2.  [Disable the current theme when a theme is interactively loaded](#org5e81fc5)
    3.  [Modeline: smart-mode-line](#org8ab1e2a)
    4.  [Fonts](#org31ee2e5)
        1.  [Ligatures](#org77dea9d)
        2.  [Unicode fonts](#orgcda6a3b)
9.  [Editing](#org14ea5cc)
    1.  [Completion](#org46827ec)
    2.  [Ignore case](#orga788d6a)
    3.  [Selection & Minibuffer](#org706315a)
        1.  [Selectrum & Prescient](#org294ff45)
    4.  [Search](#org2c0ce4c)
        1.  [CtrlF for searching](#org2ee70e6)
        2.  [Visual Regexp](#orgce9fee5)
    5.  [Undo](#orgd0935a4)
    6.  [Visual editing](#org90fff82)
        1.  [`zap-to-char` replacement](#orgaf545a8)
        2.  [Operate on a line if there's no current region](#orga6c374d)
        3.  [Expand-region](#orgc10cc77)
        4.  [Volatile highlights](#orge349e7b)
        5.  [Visual line mode](#org2537d6e)
        6.  [A better `move-beginning-of-line`](#orgb4e6549)
    7.  [Delete the selection when typing](#orga87bcd4)
    8.  [Clipboard](#org383042b)
    9.  [Tabs & Spaces](#orge8f1064)
10. [Programming](#org7ebcef6)
    1.  [Git](#org9139ba2)
    2.  [Code display](#org158c62e)
        1.  [Parentheses](#orga6fefd0)
    3.  [Line numbers](#orge880bac)
    4.  [Languages](#orgd13b98f)
        1.  [Shell](#org7b82a1d)
        2.  [Lua](#orgfe2fd7e)
        3.  [Fennel](#org3000823)
        4.  [Web](#org38d26c9)
        5.  [SSH config](#org9ae6b0f)
11. [Writing](#org100f1c7)
    1.  [Word count](#orgfbddaf6)
    2.  [Visual fill column](#org32c015a)
    3.  [Mixed-pitch](#orgcb552ec)
    4.  [Org mode](#org3025d5a)
12. [Applications](#org7eada39)
    1.  [Gemini & Gopher](#orgc966a52)
    2.  [Pastebin](#orgf849ba8)
    3.  [Gnus](#orge75cc1a)
    4.  [Nov.el: read Ebooks](#org82ec7aa)
13. [Machine-specific configurations](#orga88ef69)



<a id="org0bff216"></a>

# Preamble

I wanted to write my Emacs configuration in [Org mode](https://orgmode.org) for a while, but never could quite figure out how.  Finally, I found [Lars Tveito](https://github.com/larstvei/dot-emacs)'s config, which does exactly what I want: `init.el` is small and simple, and replaced after the first run, and `init.org` is automatically tangled.  So I'm very excited.


<a id="orged58376"></a>

# License

WTFPL.  For more info, see `LICENSE`.

Probably that's not legal under the terms of the GPL or whatever Emacs is licensed under.
SUE ME, RMS


<a id="org4c40f13"></a>

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


<a id="orgdd48fb7"></a>

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


<a id="orgf06a67b"></a>

# Early initiation

Emacs 27.1+ uses `early-init.el`, which is evaluated before things like `package.el` and other stuff.  So I have a few settings in there.


<a id="org790f346"></a>

## Preamble

Of course, first thing is the modeline.  After that, I set `load-prefer-newer` because, well, it *should*.

      ;;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-
    
    (setq load-prefer-newer t)


<a id="org27a9d06"></a>

## Computers

I have to set these constants before bootstrapping the package manager, since `straight.el` depends on Git, and at work, those are in a weird place.

    (defconst *acdw/at-work* (eq system-type 'windows-nt))
    (defconst *acdw/at-larry* (string= (system-name) "larry"))
    (defconst *acdw/at-bax* (string= (system-name) "bax"))
    (defconst *acdw/at-home* (or *acdw/at-larry* *acdw/at-bax*))


<a id="org982c51d"></a>

## Package management

I've started using straight.el, which is great.  It grabs packages from git, and apparently will let me fork and edit them, which I'll probably get around to &#x2026; eventually.


<a id="org1d43543"></a>

### At work, Git's in a weird place

    (when *acdw/at-work*
      (add-to-list 'exec-path "~/bin")
      (add-to-list 'exec-path "C:/Users/aduckworth/Downloads/PortableGit/bin"))


<a id="orgc3c01af"></a>

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


<a id="org75b502f"></a>

### [use-package](https://github.com/jwiegley/use-package)

Yeah, you know it, I know it, we all love it.  It's use-package.

    (setq straight-use-package-by-default t)
    (straight-use-package 'use-package)


<a id="orge258966"></a>

# Begin init.el

    ;;; init.el -*- lexical-binding: t; coding: utf-8 -*-
    <<tangle-on-save>>


<a id="org429e2c9"></a>

# Macros


<a id="org98b79c1"></a>

## cuss

I like `use-package`,  but I don't like doing the weird "pseudo-package" stuff a lot of people do in their emacs configs.  Partially because I have to set `:straight nil` on a lot of built-in packages, but also because I think being *that* obsessive over one interface through the whole config is &#x2026; I don't know, short-sighted?

Either way, I *do* like the `:custom` interface that `use-package` has, so I've re-implemented it in my own macro.  This way I don't have to worry about whether to `setq` or `custom-set-variable` or whatever.  Just `cuss`!

    (defmacro cuss (var val)
      "Basically `use-package''s `:custom', but without either."
      `(progn
         (funcall (or (get ',var 'custom-set) #'set-default)
                  ',var ,val)))


<a id="orgef614d8"></a>

# Files


<a id="orgcd98663"></a>

## [Keep .emacs.d tidy](https://github.com/emacscollective/no-littering)

    (straight-use-package 'no-littering)
    (require 'no-littering)


<a id="orgfa8ddd8"></a>

## Customize

I don't like the customize interface, but I still sometimes use it when I'm not sure what the name of a variable is.  So I save the stuff to a file, I just don't load it or keep track of it.

    (cuss custom-file (no-littering-expand-etc-file-name "custom.el"))


<a id="org7d9d257"></a>

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


<a id="org5a20cd6"></a>

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


<a id="org48971aa"></a>

## Backups

    (cuss backup-directory-alist
          `((".*" . ,(no-littering-expand-var-file-name "backup/"))))


<a id="org3243bbe"></a>

## [Autosave](https://github.com/bbatsov/super-save)

    (use-package super-save
      :custom
      (auto-save-default nil)
      (super-save-exclude '(".gpg"))
      :config
      (super-save-mode 1))


<a id="org1ad3687"></a>

## [Save places](https://www.emacswiki.org/emacs/SavePlace)

    (use-package saveplace
      :custom
      (save-place-file (no-littering-expand-var-file-name "places"))
      (save-place-forget-unreadable-files (not *acdw/at-work*))
      :config
      (save-place-mode 1))


<a id="orge49f590"></a>

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


<a id="orgbaf79a4"></a>

# User interface


<a id="org57b0df2"></a>

## Look


<a id="org7eea925"></a>

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


<a id="orgb767f7c"></a>

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


<a id="org8983905"></a>

### Cursor

    (cuss cursor-type 'bar)
    (cuss cursor-in-non-selected-windows 'hollow)
    (blink-cursor-mode 0)


<a id="org62c9668"></a>

### Interactivity

1.  Mouse

        (cuss mouse-yank-at-point t)

2.  Dialogs

        (cuss use-dialog-box nil)

3.  Disabled functions

        (cuss disabled-command-function nil)

4.  Function aliases

        (fset 'yes-or-no-p #'y-or-n-p)


<a id="org1ea822c"></a>

### Miscellaneous

1.  Convert `^L` to a line

        (use-package page-break-lines
          :config
          (global-page-break-lines-mode 1))


<a id="org677f380"></a>

## Themes: [Modus](https://github.com/protesilaos/modus-themes)

    (use-package modus-operandi-theme)
    (use-package modus-vivendi-theme)


<a id="orgb821d6f"></a>

### [Change themes](https://github.com/hadronzoo/theme-changer) based on time of day

    (use-package theme-changer
      :init
      (setq calendar-location-name "Baton Rouge, LA"
            calendar-latitude 30.39
            calendar-longitude -91.83)
      :config
      (change-theme 'modus-operandi 'modus-vivendi))


<a id="org5e81fc5"></a>

### Disable the current theme when a theme is interactively loaded

This doesn't happen often, but I'll be ready when it does.

    (defadvice load-theme
        (before disable-before-load (theme &optional no-confirm no-enable) activate)
      (mapc 'disable-theme custom-enabled-themes))


<a id="org8ab1e2a"></a>

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


<a id="org31ee2e5"></a>

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


<a id="org77dea9d"></a>

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


<a id="orgcda6a3b"></a>

### [Unicode fonts](https://github.com/rolandwalker/unicode-fonts)

    (use-package persistent-soft)
    
    (use-package unicode-fonts
      :after persistent-soft
      :config
      (unicode-fonts-setup))


<a id="org14ea5cc"></a>

# Editing


<a id="org46827ec"></a>

## Completion

I was using company, but I think it might've been causing issues with `awk-mode`, so I'm trying `hippie-mode` right now.  So far, I'm also enjoying not having a popup all the time.

    (bind-key "M-/" #'hippie-expand)


<a id="orga788d6a"></a>

## Ignore case

    (cuss completion-ignore-case t)
    (cuss read-buffer-completion-ignore-case t)
    (cuss read-file-name-completion-ignore-case t)


<a id="org706315a"></a>

## Selection & Minibuffer


<a id="org294ff45"></a>

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


<a id="org2c0ce4c"></a>

## Search


<a id="org2ee70e6"></a>

### CtrlF for searching

    (use-package ctrlf
      :custom
      (ctrlf-show-match-count-at-eol nil)
      :config
      (ctrlf-mode +1)
      :bind
      ("C-s" . ctrlf-forward-regexp))


<a id="orgce9fee5"></a>

### [Visual Regexp](https://github.com/benma/visual-regexp.el)

    (use-package visual-regexp
      :bind
      ([remap query-replace] . 'vr/query-replace))


<a id="orgd0935a4"></a>

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


<a id="org90fff82"></a>

## Visual editing


<a id="orgaf545a8"></a>

### `zap-to-char` replacement

    (use-package zop-to-char
      :bind
      ([remap zap-to-char] . zop-to-char)
      ([remap zap-up-to-char] . zop-up-to-char))


<a id="orga6c374d"></a>

### Operate on a line if there's no current region

    (use-package whole-line-or-region
      :config
      (whole-line-or-region-global-mode +1))


<a id="orgc10cc77"></a>

### Expand-region

    (use-package expand-region
      :bind
      ("C-=" . er/expand-region)
      ("C-+" . er/contract-region))


<a id="orge349e7b"></a>

### Volatile highlights

    (use-package volatile-highlights
      :config
      (volatile-highlights-mode 1))


<a id="org2537d6e"></a>

### Visual line mode

    (global-visual-line-mode 1)


<a id="orgb4e6549"></a>

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


<a id="orga87bcd4"></a>

## Delete the selection when typing

    (delete-selection-mode 1)


<a id="org383042b"></a>

## Clipboard

    (cuss save-interprogram-paste-before-kill t)


<a id="orge8f1064"></a>

## Tabs & Spaces

    (cuss indent-tabs-mode nil)
    (cuss sentence-end-double-space t)


<a id="org7ebcef6"></a>

# Programming


<a id="org9139ba2"></a>

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


<a id="org158c62e"></a>

## Code display

    (add-hook 'prog-mode-hook #'prettify-symbols-mode)


<a id="orga6fefd0"></a>

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


<a id="orge880bac"></a>

## Line numbers

    (add-hook 'prog-mode-hook
              (if (and (fboundp 'display-line-numbers-mode)
                       (display-graphic-p))
                  #'display-line-numbers-mode
                #'linum-mode))


<a id="orgd13b98f"></a>

## Languages


<a id="org7b82a1d"></a>

### Shell

    (use-package shfmt
      :custom
      (shfmt-arguments '("-i" "4" "-ci"))
      :hook
      (sh-mode . shfmt-on-save-mode))
    
    ;; fish
    (use-package fish-mode)


<a id="orgfe2fd7e"></a>

### Lua

    (use-package lua-mode
      :mode "\\.lua\\'"
      :interpreter "lua")


<a id="org3000823"></a>

### Fennel

    (use-package fennel-mode
      :mode "\\.fnl\\'")


<a id="org38d26c9"></a>

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


<a id="org9ae6b0f"></a>

### SSH config

    (use-package ssh-config-mode)


<a id="org100f1c7"></a>

# Writing


<a id="orgfbddaf6"></a>

## Word count

    (use-package wc-mode
      :init
      (rm-whitelist-add "WC")
      :hook
      (text-mode . wc-mode))


<a id="org32c015a"></a>

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


<a id="orgcb552ec"></a>

## Mixed-pitch

    (use-package mixed-pitch
      :hook
      (text-mode . mixed-pitch-mode))


<a id="org3025d5a"></a>

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


<a id="org7eada39"></a>

# Applications


<a id="orgc966a52"></a>

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


<a id="orgf849ba8"></a>

## Pastebin

    (use-package 0x0
      :custom
      (0x0-default-service 'ttm))


<a id="orge75cc1a"></a>

## Gnus

    (cuss gnus-select-method
          '(nnimap "imap.fastmail.com"
                   (nnimap-inbox "INBOX")
                   (nnimap-split-methods default)
                   (nnimap-expunge t)
                   (nnimap-stream ssl)))
    
    (cuss gnus-secondary-select-methods
          '((nntp "news.gwene.org")))


<a id="org82ec7aa"></a>

## Nov.el: read Ebooks

    (use-package nov
      :mode ("\\.epub\\'" . nov-mode)
      :custom
      (nov-text-width t)
      :hook
      (nov-mode . visual-line-mode)
      (nov-mode . visual-fill-column-mode))


<a id="orga88ef69"></a>

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

