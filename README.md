
# Table of Contents

1.  [Pave the way](#org24d31f9)
    1.  [Correct `exec-path`](#org82dd805)
    2.  [Package management](#org947e1de)
        1.  [Straight.el](#orgd711f6b)
        2.  [Use-package](#org9392b5d)
        3.  [Extra use-package keywords](#orgc93ae09)
    3.  [Customize variables](#org7cae7fe)
        1.  [Put customizations in a separate file](#org126d855)
        2.  [A macro for ease of customization](#org2cf1d1a)
    4.  [Keep a tidy `~/.emacs`](#orga6c0096)
2.  [Look and Feel](#org1ecbcc5)
    1.  [Simplify the UI](#org23fb19e)
        1.  [Tool bars and menu bars](#orgad64258)
        2.  [Scroll bars](#org9b2f49e)
        3.  [Dialog boxen](#orgf1c5f65)
        4.  [Shorten confirmations](#orgedf9e78)
        5.  [Remove the bell](#org1643ce2)
        6.  [Tell Ediff to setup windows better](#org3996a6f)
    2.  [Tweak the remaining UI](#orgcdf874b)
        1.  [Window dividers](#org187505c)
        2.  [Fringes](#org3fd2bc6)
        3.  [Minibuffer](#org8ff32ea)
        4.  [Tabs](#orgef2a000)
        5.  [Cursor](#orge57d1b2)
        6.  [Buffer names](#orgb3f29a9)
        7.  [Buffer boundaries](#org2627b1e)
    3.  [Startup](#org1fc3c6d)
    4.  [Theme](#org207a1bd)
        1.  [Fonts](#org52f6c8c)
3.  [Interactivity](#org6cbcfe5)
    1.  [Selectrum](#org7f26398)
    2.  [Prescient](#orgea8df9e)
    3.  [Consult](#org8818eb9)
    4.  [Marginalia](#orgd31a964)
    5.  [Ignore case](#org6e4913f)
    6.  [Search](#org416dd18)
4.  [Persistence](#orgb20768d)
    1.  [Save history](#org7dfee32)
    2.  [Save places in files](#org0f20005)
    3.  [Recent files](#org6d1a477)
        1.  [Easily navigate recent files](#org9368a6b)
    4.  [Undo](#orgbb4f91a)
5.  [Editing](#org52b008a)
    1.  [Operate visually on lines](#orgce838ba)
    2.  [Require a final newline](#org6f67996)
    3.  [Killing & Yanking](#orga2bdb3e)
        1.  [Replace selection when typing](#org16c1a6b)
        2.  [Save existing clipboard text into kill ring before replacing it](#orgea7fd73)
    4.  [So long mode](#org27f430f)
6.  [Files](#org8cc8ee8)
    1.  [Encoding](#org8ca2e9b)
        1.  [UTF-8](#org54363a7)
        2.  [Convert all files to UNIX-style line endings](#orgeaed3bd)
    2.  [Backups](#org7239c47)
    3.  [Auto-saves](#org32fc658)
    4.  [Revert files](#org94456e2)
    5.  [Add a timestamp to files](#orgb586b3b)
7.  [Programming](#org738fbd9)
    1.  [Which function are we in?](#org080eb2f)
8.  [Writing](#org6bf5097)
    1.  [Visual Fill Column](#org6f01971)
    2.  [Type nice-looking quote-type marks](#org03e747d)
9.  [Applications](#org9528516)
    1.  [Magit](#orgd2a60aa)
10. [Appendices](#org7339cf2)
    1.  [Emacs' files](#org6070b2c)
        1.  [init.el](#org0d720f6)
        2.  [early-init.el](#orgd6bffd2)
    2.  [Ease tangling and loading of Emacs' init](#org9c5b437)
    3.  [License](#org1a4bb4d)
        1.  [Note on the license](#orga6047ee)



<a id="org24d31f9"></a>

# Pave the way


<a id="org82dd805"></a>

## Correct `exec-path`

    (let ((win-downloads "c:/Users/aduckworth/Downloads"))
      (dolist (path `(;; Linux
    		  ,(expand-file-name "bin"
    				     user-emacs-directory)
    		  ,(expand-file-name "~/bin")
    		  ,(expand-file-name "~/.local/bin")
    		  ,(expand-file-name "~/Scripts")
    		  ;; Windows
    		  ,(expand-file-name "emacs/bin"
    				    win-downloads)
    		  ,(expand-file-name "PortableGit/bin"
    				    win-downloads)
    		  ,(expand-file-name "PortableGit/usr/bin"
    				    win-downloads)))
        (when (file-exists-p path)
          (add-to-list 'exec-path path))))


<a id="org947e1de"></a>

## Package management


<a id="orgd711f6b"></a>

### Straight.el

    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
    	(url-retrieve-synchronously
    	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
    	 'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))


<a id="org9392b5d"></a>

### Use-package

    (setq straight-use-package-by-default t)
    (straight-use-package 'use-package)


<a id="orgc93ae09"></a>

### Extra use-package keywords

1.  :custom-update

        (straight-use-package
         '(use-package-custom-update
           :host "github"
           :repo "a13/use-package-custom-update"))
        
        (require 'use-package-custom-update)


<a id="org7cae7fe"></a>

## Customize variables


<a id="org126d855"></a>

### Put customizations in a separate file

    (setq custom-file
          (expand-file-name "custom.el" user-emacs-directory))


<a id="org2cf1d1a"></a>

### A macro for ease of customization

    (defmacro cuss (var val &optional docstring)
      "Basically `:custom' from `use-package', broken out."
      `(funcall (or (get ',var 'custom-set) #'set-default)
    	    ',var ,val))


<a id="orga6c0096"></a>

## Keep a tidy `~/.emacs`

    (use-package no-littering
      :custom
      (backup-directory-alist
       `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
      (auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "autosaves/") t)))
      (save-place-file
       (no-littering-expand-var-file-name "places"))
      (undo-fu-session-directory
       (no-littering-expand-var-file-name "undos/"))
      (undohist-directory
       (no-littering-expand-var-file-name "undos/"))
      (elpher-certificate-directory
       (no-littering-expand-var-file-name "elpher-certificates/")))
    
    (dolist (dir '("backup"
    	       "autosaves"
    	       "undos"
    	       "elpher-certificates"))
      (make-directory (no-littering-expand-var-file-name dir) t))


<a id="org1ecbcc5"></a>

# Look and Feel


<a id="org23fb19e"></a>

## Simplify the UI


<a id="orgad64258"></a>

### Tool bars and menu bars

    (cuss default-frame-alist
          '((tool-bar-lines . 0)
    	(menu-bar-lines . 0)))
    
    (menu-bar-mode -1)
    (tool-bar-mode -1)


<a id="org9b2f49e"></a>

### Scroll bars

    (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
    (scroll-bar-mode -1)
    
    (add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
    (horizontal-scroll-bar-mode -1)


<a id="orgf1c5f65"></a>

### Dialog boxen

    (cuss use-dialog-box nil)


<a id="orgedf9e78"></a>

### Shorten confirmations

    (fset 'yes-or-no-p #'y-or-n-p)


<a id="org1643ce2"></a>

### Remove the bell

    (cuss visible-bell (not (string= (system-name) "larry")))


<a id="org3996a6f"></a>

### Tell Ediff to setup windows better

    (declare-function ediff-setup-windows-plain "ediff-wind.el")
    (cuss ediff-window-setup-function #'ediff-setup-windows-plain)


<a id="orgcdf874b"></a>

## Tweak the remaining UI


<a id="org187505c"></a>

### Window dividers

    (add-to-list 'default-frame-alist '(right-divider-width . 2))
    (add-to-list 'default-frame-alist '(bottom-divider-width . 2))


<a id="org3fd2bc6"></a>

### Fringes

    (add-to-list 'default-frame-alist '(left-fringe-width . 2))
    (add-to-list 'default-frame-alist '(right-fringe-width . 2))


<a id="org8ff32ea"></a>

### Minibuffer

1.  Setup the minibuffer frame

        (cuss minibuffer-frame-alist
              '((width . 80)
        	(height . 2)
        	(vertical-scrollbars . nil)))
        
        (set-window-scroll-bars (minibuffer-window) nil nil)

2.  Keep the cursor from going into the prompt

        (cuss minibuffer-prompt-properties
              '(read-only t cursor-intangible t face minibuffer-prompt))


<a id="orgef2a000"></a>

### Tabs

1.  Show the tabs as current buffer, plus window count

        (cuss tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)

2.  Only show the tab bar when there's more than one tab

        (cuss tab-bar-show 1)


<a id="orge57d1b2"></a>

### Cursor

    (cuss cursor-type 'bar)
    (cuss cursor-in-non-selected-windows 'hollow)


<a id="orgb3f29a9"></a>

### Buffer names

    (require 'uniquify)
    (cuss uniquify-buffer-name-style 'forward)


<a id="org2627b1e"></a>

### Buffer boundaries

    (cuss indicate-buffer-boundaries
          '((top . right)
    	(bottom . right)
    	(t . nil)))
    
    (cuss indicate-empty-lines t)


<a id="org1fc3c6d"></a>

## Startup

    (cuss inhibit-startup-buffer-menu t)
    (cuss inhibit-start-screen t)
    (cuss initial-buffer-choice t)
    (cuss initial-scratch-message ";; Hi there!\n")


<a id="org207a1bd"></a>

## Theme

    (use-package modus-themes
      :straight (modus-themes
    	     :host gitlab
    	     :repo "protesilaos/modus-themes"
    	     :branch "main")
      :custom
      (modus-themes-slanted-constructs t)
      (modus-themes-bold-constructs t)
      (modus-themes-fringes nil)
      (modus-themes-mode-line '3d)
      (modus-themes-syntax 'yellow-comments)
      (modus-themes-intense-hl-line nil)
      (modus-themes-paren-match 'intense-bold)
      (modus-themes-links nil)
      (modus-themes-no-mixed-fonts nil)
      (modus-themes-prompts nil)
      (modus-themes-completions nil)
      (modus-themes-diffs nil)
      (modus-themes-org-blocks 'grayscale)
      (modus-themes-headings
       '())
      (modus-themes-variable-pitch-headings t)
      (modus-themes-scale-headings t)
      (modus-themes-scale-1 1.1)
      (modus-themes-scale-2 1.15)
      (modus-themes-scale-3 1.21)
      (modus-themes-scale-4 1.27)
      (modus-themes-scale-5 1.33)
      :custom-face
      (font-lock-comment-face
       ((t (:inherit (custom-comment italic variable-pitch)))))
      :init
      (load-theme 'modus-operandi t))


<a id="org52f6c8c"></a>

### Fonts

1.  Define fonts

        (defun font-candidate (&rest fonts)
          (catch :font
            (dolist (font fonts)
              (if (find-font (font-spec :name font))
        	  (throw :font font)))))
        
        (defun acdw/setup-fonts ()
          "Setup fonts.  This has to happen after the frame is setup for
        the first time, so it should be added to `window-setup-hook'.  It
        removes itself from that hook."
          (interactive)
          (set-face-attribute 'default nil
        		      :font
        		      (font-candidate
        		       "Libertinus Mono-11"
        		       "Linux Libertine Mono O-11"
        		       "Go Mono-10"
        		       "Consolas-10"))
        
          (set-face-attribute 'fixed-pitch nil
        		      :font
        		      (font-candidate
        		       "Libertinus Mono-11"
        		       "Linux Libertine Mono O-11"
        		       "Go Mono-10"
        		       "Consolas-10"))
        
          (set-face-attribute 'variable-pitch nil
        		      :font
        		      (font-candidate
        		       "Libertinus Serif-13"
        		       "Linux Libertine O-12"
        		       "Georgia-11"))
        
          (remove-hook 'window-setup-hook #'acdw/setup-fonts))
        
        (add-hook 'window-setup-hook #'acdw/setup-fonts)

2.  Variable-pitch in text modes

        (add-hook 'text-mode-hook #'variable-pitch-mode)

3.  Line spacing

        (cuss line-spacing 0.1)

4.  Unicode fonts

        (use-package unicode-fonts
          :config
          (unicode-fonts-setup))


<a id="org6cbcfe5"></a>

# Interactivity


<a id="org7f26398"></a>

## Selectrum

    (use-package selectrum
      :config
      (selectrum-mode +1))


<a id="orgea8df9e"></a>

## Prescient

    (use-package prescient
      :config
      (prescient-persist-mode +1))
    
    (use-package selectrum-prescient
      :after (selectrum prescient)
      :config
      (selectrum-prescient-mode +1))


<a id="org8818eb9"></a>

## Consult

    (use-package consult
      :after (selectrum)
      :straight (consult
    	     :host github
    	     :repo "minad/consult")
      :bind
      (("C-x b" . consult-buffer)
       ("C-x 4 b" . consult-buffer-other-window)
       ("C-x 5 b" . consult-buffer-other-frame)
       ("M-g o" . consult-outline)
       ("M-g l" . consult-line)
       ("M-y" . consult-yank-pop)
       ("<help> a" . consult-apropos))
      :init
      (fset 'multi-occur #'consult-multi-occur))


<a id="orgd31a964"></a>

## Marginalia

    (use-package marginalia
      :straight (marginalia
    	     :host github
    	     :repo "minad/marginalia"
    	     :branch "main")
      :custom
      (marginalia-annotators
       '((command . marginalia-annotate-command-full)
         (customize-group . marginalia-annotate-customize-group)
         (variable . marginalia-annotate-variable)
         (face . marginalia-annotate-face)
         (symbol . marginalia-annotate-symbol)
         (variable . marginalia-annotate-variable)
         (package . marginalia-annotate-package)))
      :init
      (marginalia-mode +1))


<a id="org6e4913f"></a>

## Ignore case

    (cuss completion-ignore-case t)
    (cuss read-buffer-completion-ignore-case t)
    (cuss read-file-name-completion-ignore-case t)


<a id="org416dd18"></a>

## Search

    (use-package ctrlf
      :custom
      (ctrlf-show-match-count-at-eol nil)
      :bind
      ("C-s" . ctrlf-forward-regexp)
      ("C-r" . ctrlf-backward-regexp)
      ("C-M-s" . ctrlf-forward-literal)
      ("C-M-r" . ctrlf-backward-literal)
      :config
      (ctrlf-mode +1))


<a id="orgb20768d"></a>

# Persistence


<a id="org7dfee32"></a>

## Save history

    (require 'savehist)
    
    (cuss savehist-additional-variables
          '(kill-ring
    	search-ring
    	regexp-search-ring))
    
    (cuss savehist-save-minibuffer-history t)
    
    (cuss history-length t)
    
    (cuss history-delete-duplicates t)
    
    (savehist-mode +1)


<a id="org0f20005"></a>

## Save places in files

    (require 'saveplace)
    
    (cuss save-place-forget-unreadable-files
          (not (eq system-type 'windows-nt)))
    
    (save-place-mode 1)


<a id="org6d1a477"></a>

## Recent files

    (require 'recentf)
    
    (cuss recentf-max-menu-items 100)
    (cuss recentf-max-saved-items 100)
    
    (with-eval-after-load 'no-littering
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory))
    
    (recentf-mode 1)


<a id="org9368a6b"></a>

### Easily navigate recent files

    (defun recentf-find-file ()
      "Find a recent file using `completing-read'."
      (interactive)
      (let ((file (completing-read "Recent file: " recentf-list nil t)))
        (when file
          (find-file file))))
    
    (global-set-key (kbd "C-x C-r") #'recentf-find-file)


<a id="orgbb4f91a"></a>

## Undo

    (use-package undohist
      :config
      (undohist-initialize))


<a id="org52b008a"></a>

# Editing


<a id="orgce838ba"></a>

## Operate visually on lines

    (global-visual-line-mode +1)


<a id="org6f67996"></a>

## Require a final newline

    (cuss require-final-newline t)


<a id="orga2bdb3e"></a>

## Killing & Yanking


<a id="org16c1a6b"></a>

### Replace selection when typing

    (delete-selection-mode +1)


<a id="orgea7fd73"></a>

### Save existing clipboard text into kill ring before replacing it

    (cuss save-interprogram-paste-before-kill t)


<a id="org27f430f"></a>

## So long mode

    (when (fboundp 'global-so-long-mode)
      (global-so-long-mode))


<a id="org8cc8ee8"></a>

# Files


<a id="org8ca2e9b"></a>

## Encoding


<a id="org54363a7"></a>

### UTF-8

    (set-language-environment 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (cuss locale-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)


<a id="orgeaed3bd"></a>

### Convert all files to UNIX-style line endings

from [Emacs Wiki](https://www.emacswiki.org/emacs/EndOfLineTips).

    (defun ewiki/no-junk-please-were-unixish ()
      "Convert line endings to UNIX, dammit."
      (let ((coding-str (symbol-name buffer-file-coding-system)))
        (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
          (set-buffer-file-coding-system 'unix))))

I add it to the `find-file-hook` *and* `before-save-hook` because I don't want to ever work with anything other than UNIX line endings ever again.  I just don't care.  Even Microsoft Notepad can handle UNIX line endings, so I don't want to hear it.

    (add-hook 'find-file-hook #'ewiki/no-junk-please-were-unixish)
    (add-hook 'before-save-hook #'ewiki/no-junk-please-were-unixish)


<a id="org7239c47"></a>

## Backups

    (cuss backup-by-copying 1)
    (cuss delete-old-versions -1)
    (cuss version-control t)
    (cuss vc-make-backup-files t)


<a id="org32fc658"></a>

## Auto-saves

    (auto-save-visited-mode 1)


<a id="org94456e2"></a>

## Revert files

    (cuss auto-revert-verbose nil)
    (global-auto-revert-mode +1)


<a id="orgb586b3b"></a>

## Add a timestamp to files

    (add-hook 'before-save-hook #'time-stamp)


<a id="org738fbd9"></a>

# Programming


<a id="org080eb2f"></a>

## Which function are we in?

    (which-function-mode +1)


<a id="org6bf5097"></a>

# Writing


<a id="org6f01971"></a>

## Visual Fill Column

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


<a id="org03e747d"></a>

## Type nice-looking quote-type marks

    (use-package typo
      :hook
      (text-mode . typo-mode))


<a id="org9528516"></a>

# Applications


<a id="orgd2a60aa"></a>

## Magit

    (use-package magit
      :bind
      ("C-x g" . magit-status))


<a id="org7339cf2"></a>

# Appendices


<a id="org6070b2c"></a>

## Emacs' files


<a id="org0d720f6"></a>

### init.el

    ;; init.el -*- lexical-binding: t -*-

1.  Load config

    from [Protesilaos Stavrou](https://protesilaos.com/dotemacs/#h:584c3604-55a1-49d0-9c31-abe46cb1f028).
    
        (let* ((conf (expand-file-name "config"
        			       user-emacs-directory))
               (elc (concat conf ".elc"))
               (el (concat conf ".el"))
               (org (concat conf ".org")))
          (cond ((file-exists-p elc) (load-file elc))
        	((file-exists-p el) (load-file el))
        	(t (require 'org)
        	   (org-babel-load-file org))))


<a id="orgd6bffd2"></a>

### early-init.el

    ;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

    (setq load-prefer-newer t)
    (setq frame-inhibit-implied-resize t)


<a id="org9c5b437"></a>

## Ease tangling and loading of Emacs' init

    (defun acdw/tangle-and-load-init ()
      (interactive)
      "If the current buffer is `config.org', tangle it, then byte-compile."
      (let ((config (expand-file-name "config.org" user-emacs-directory)))
        (when (string= (buffer-file-name) config)
          (let ((prog-mode-hook nil))
    	(require 'org)
    	(org-babel-tangle-file config)
    	(org-md-export-to-markdown)
    
          (dolist (file `(,(expand-file-name "init.el" 
    					 user-emacs-directory)
    		      ,(expand-file-name "config.el"
    					 user-emacs-directory)))
    	(byte-compile-file file t))))))
    
    (add-hook 'after-save-hook #'acdw/tangle-and-load-init)


<a id="org1a4bb4d"></a>

## License

Copyright © 2020 Case Duckworth <acdw@acdw.net>

This work is free.  You can redistribute it and/or modify it under the
terms of the Do What the Fuck You Want To Public License, Version 2,
as published by Sam Hocevar.  See the `LICENSE` file, tangled from the
following source block, for details.

    DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
    
    Version 2, December 2004
    
    Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
    
    Everyone is permitted to copy and distribute verbatim or modified copies of
    this license document, and changing it is allowed as long as the name is changed.
    
    DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
    
    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
    
       0. You just DO WHAT THE FUCK YOU WANT TO.


<a id="orga6047ee"></a>

### Note on the license

It's highly likely that the WTFPL is completely incompatible with the
GPL, for what should be fairly obvious reasons.  To that, I say:

**SUE ME, RMS!**

