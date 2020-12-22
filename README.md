Let’s configure Emacs using Org mode, they said.  It’ll be fun, they said.


# Pave the way


## Correct `exec-path`

    (let ((win-downloads "c:/Users/aduckworth/Downloads"))
      (dolist (path (list
    		 ;; Linux
    		 (expand-file-name "bin"
    				   user-emacs-directory)
    		 (expand-file-name "~/bin")
    		 (expand-file-name "~/.local/bin")
    		 (expand-file-name "~/Scripts")
    		 ;; Windows
    		 (expand-file-name "emacs/bin"
    				   win-downloads)
    		 (expand-file-name "m/usr/bin"
    				   win-downloads)
    		 (expand-file-name "m/mingw64/bin"
    				   win-downloads)
    		 (expand-file-name "PortableGit/bin"
    				   win-downloads)
    		 (expand-file-name "PortableGit/usr/bin"
    				   win-downloads)))
        (when (file-exists-p path)
          (add-to-list 'exec-path path))))


## Package management


### Straight.el

Since for whatever reason, Straight can't bootstrap itself on Windows
&#x2013; I've wrapped it in a function here and added the direct git command
when it errors.

    (defun acdw/bootstrap-straight ()
      (defvar bootstrap-version)
    		       (let ((bootstrap-file
    			      (expand-file-name
    			       "straight/repos/straight.el/bootstrap.el"
    			       user-emacs-directory))
    			     (bootstrap-version 5))
    			 (unless (file-exists-p bootstrap-file)
    			   (with-current-buffer
    			       (url-retrieve-synchronously
    				(concat "https://raw.githubusercontent.com/"
    					"raxod502/straight.el/develop/install.el")
    				'silent 'inhibit-cookies)
    			     (goto-char (point-max))
    			     (eval-print-last-sexp)))
    			 (load bootstrap-file nil 'nomessage)))
    
    (unless (ignore-errors (acdw/bootstrap-straight))
      (message "Straight.el didn't bootstrap correctly.  Cloning directly...")
      (call-process "git" nil (get-buffer-create "*bootstrap-straight-messages*") nil
    		"clone"
    		"https://github.com/raxod502/straight.el"
    		(expand-file-name "straight/repos/straight.el"
    				  user-emacs-directory))
      (acdw/bootstrap-straight))


### Use-package

    (setq straight-use-package-by-default t)
    (setq use-package-hook-name-suffix nil)
    (straight-use-package 'use-package)


### Extra use-package keywords

1.  :custom-update

        (straight-use-package
         '(use-package-custom-update
           :host github
           :repo "a13/use-package-custom-update"))
        
        (require 'use-package-custom-update)


## Customize variables


### Put customizations in a separate file

    (setq custom-file
          (expand-file-name "custom.el" user-emacs-directory))


### A macro for ease of customization

    (defmacro cuss (var val &optional docstring)
      "Basically `:custom' from `use-package', broken out."
      `(funcall (or (get ',var 'custom-set) #'set-default)
    	    ',var ,val))


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
      (elpher-certificate-directory
       (no-littering-expand-var-file-name "elpher-certificates/")))
    
    (dolist (dir '("backup"
    	       "autosaves"
    	       "undos"
    	       "elpher-certificates"))
      (make-directory (no-littering-expand-var-file-name dir) 'parents))


## About me

    (setq user-full-name "Case Duckworth"
          user-mail-address "acdw@acdw.net")


# Look and Feel


## Simplify the UI


### Tool bars and menu bars

    (cuss default-frame-alist
          '((tool-bar-lines . 0)
    	(menu-bar-lines . 0)))
    
    (menu-bar-mode -1)
    (tool-bar-mode -1)


### Scroll bars

    (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
    (scroll-bar-mode -1)
    
    (add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
    (horizontal-scroll-bar-mode -1)


### Dialog boxen

    (cuss use-dialog-box nil)


### Shorten confirmations

    (fset 'yes-or-no-p #'y-or-n-p)


### Remove the bell

    (cuss visible-bell (not (string= (system-name) "larry")))
    
    (defun acdw/ring-bell-function ()
      "Custom bell-ringing function."
      (let ((orig-face (face-foreground 'mode-line)))
        (set-face-foreground 'modeline "#F2804F")
        (run-with-idle-timer
         0.1 nil
         (lambda (fg)
           (set-face-foreground 'mode-line fg))
         orig-face)))
    
    (cuss ring-bell-function #'acdw/ring-bell-function)


### Tell Ediff to setup windows better

    (declare-function ediff-setup-windows-plain "ediff-wind.el")
    (cuss ediff-window-setup-function #'ediff-setup-windows-plain)


## Tweak the remaining UI


### Fringes

    (add-to-list 'default-frame-alist '(left-fringe-width . 2))
    (add-to-list 'default-frame-alist '(right-fringe-width . 2))


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


### Tabs

1.  Show the tabs as current buffer, plus window count

        (cuss tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)

2.  Only show the tab bar when there's more than one tab

        (cuss tab-bar-show 1
              "Show the tab bar only when there's more than 1 tab.")


### Cursor

    (cuss cursor-type 'bar
          "Show a vertical bar for the cursor.")
    (cuss cursor-in-non-selected-windows 'hollow
          "In inactive windows, make the cursor an empty box.")
    (blink-cursor-mode 0)


### Buffer names

    (require 'uniquify)
    (cuss uniquify-buffer-name-style 'forward)


### Buffer boundaries

    (cuss indicate-buffer-boundaries
          '((top . right)
    	(bottom . right)
    	(t . nil)))
    
    (cuss indicate-empty-lines t)


## Windows


### Split windows *more* sensibly

from [Stack Overflow](https://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in).

    (defun my-split-window-sensibly (&optional window)
      (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
    	     ;; Split window horizontally.
    	     (with-selected-window window
    	       (split-window-right)))
    	(and (window-splittable-p window)
    	     ;; Split window vertically.
    	     (with-selected-window window
    	       (split-window-below)))
    	(and (eq window (frame-root-window (window-frame window)))
    	     (not (window-minibuffer-p window))
    	     ;; If WINDOW is the only window on its frame and is not the
    	     ;; minibuffer window, try to split it horizontally disregarding
    	     ;; the value of `split-width-threshold'.
    	     (let ((split-width-threshold 0))
    	       (when (window-splittable-p window t)
    		 (with-selected-window window
    		   (split-window-right))))))))
    
    (setq split-window-preferred-function #'my-split-window-sensibly)


### Winner mode

    (when (fboundp 'winner-mode)
      (winner-mode +1))


### Windmove

    (cuss windmove-create-window t
          "Create windows in a direction if they don't exist.")
    (cuss windomove-wrap-around t
          "Wrap window movements around frame edges.")
    
    (windmove-default-keybindings)


### Pop some buffers up in the same window

from [link0ff](https://github.com/link0ff/emacs-init).

    (push `(,(rx bos
    	     "*"
    	     (or "Help" "Apropos" "Colors" "Buffer List" "Command History"
    		 "Dictionary" "Locate" "Messages" "Proced" "eww" "snd"
    		 (and "gud-" (+ (any "a-z0-9")))
    		 "compilation" "grep" "erlang" "haskell"
    		 ;; Handle both "*shell*" and e.g. "*emacs-shell*"
    		 ;; generated by `project-shell':
    		 (and (? (* nonl) "-") "shell")
    		 "Shell Command Output"
    		 (and "SQL: " (+ (any "A-za-z")))
    		 "Diff" "vc-dir" "vc-log" "vc-search-log")
    	     "*"
    	     ;; Uniquifed buffer name with optional suffix in angle brackets
    	     (? (and "<" (+ (not (any ">"))) ">"))
    	     eos)
    	display-buffer-same-window
    	(inhibit-same-window . nil))
          display-buffer-alist)
    
    (defun display-buffer-from-help-p (_buffer-name _action)
      (unless current-prefix-arg
        (with-current-buffer (window-buffer)
          (eq major-mode 'help-mode))))
    
    (push '(display-buffer-from-help-p display-buffer-same-window)
          display-buffer-alist)


## Startup

    (cuss inhibit-startup-screen t "Don't show Emacs' startup buffer.")
    (cuss initial-buffer-choice t "Start at *scratch*.")
    (cuss initial-scratch-message "" "Empty *scratch*.")


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
       '((1 . line)
         (t . t)))
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


### Change theme based on time of day

    (cuss calendar-latitude 30.4515)
    (cuss calendar-longitude -91.1871)
    
    (use-package circadian
      :custom
      (circadian-themes '((:sunrise . modus-operandi)
    		      (:sunset . modus-vivendi)))
      :config
      (circadian-setup))


### Modeline

    (use-package mood-line
      :config
      (mood-line-mode +1))


### Fonts

1.  Define fonts

        (defun font-candidate (&rest fonts)
          (catch :font
            (dolist (font fonts)
              (if (find-font (font-spec :name font))
        	  (throw :font font)))))
        
        (defun set-face-from-alternatives (face fonts)
          (dolist (font fonts)
            (if (find-font (font-spec :family (car font)))
        	(apply #'set-face-attribute `(,face nil
        					    :family ,(car font)
        					    ,@(cdr font))))))
        
        (defun acdw/setup-fonts ()
          "Setup fonts.  This has to happen after the frame is setup for
        the first time, so it should be added to `window-setup-hook'.  It
        removes itself from that hook."
          (interactive)
          (when (display-graphic-p)
            (set-face-from-alternatives 'default
        				'(("Libertinus Mono"
        				   :height 110)
        				  ("Linux Libertine Mono O"
        				   :height 110)
        				  ("Go Mono"
        				   :height 100)
        				  ("Consolas"
        				   :height 100)))
        
            (set-face-from-alternatives 'fixed-pitch
        				'(("Libertinus Mono"
        				   :height 1.0)
        				  ("Linux Libertine Mono O"
        				   :height 1.0)
        				  ("Go Mono"
        				   :height 1.0)
        				  ("Consolas"
        				   :height 1.0)))
        
            (set-face-from-alternatives 'variable-pitch
        				'(("Libertinus Serif"
        				   :height 1.0)
        				  ("Linux Libertine O"
        				   :height 1.0)
        				  ("Georgia"
        				   :height 1.0)))
        
            (remove-function after-focus-change-function #'acdw/setup-fonts)))
        
        (add-function :before after-focus-change-function #'acdw/setup-fonts)

2.  Variable-pitch in text modes

        (add-hook 'text-mode-hook #'variable-pitch-mode)

3.  Line spacing

        (cuss line-spacing 0.1)

4.  Unicode fonts

        (use-package unicode-fonts
          :config
          (unicode-fonts-setup))


# Interactivity


## Async

    (use-package async)
    
    (autoload 'dired-async-mode "dired-async.el" nil t)
    (dired-async-mode +1)
    
    (async-bytecomp-package-mode +1)


## Completing-read


### Shadow file names

    (cuss file-name-shadow-properties
          '(invisible t))
    
    (file-name-shadow-mode +1)


### Selectrum

    (use-package selectrum
      :config
      (selectrum-mode +1))


### Prescient

    (use-package prescient
      :config
      (prescient-persist-mode +1))
    
    (use-package selectrum-prescient
      :after (selectrum prescient)
      :config
      (selectrum-prescient-mode +1))


### Consult

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
    
    (use-package consult-selectrum
      :straight (consult-selectrum
    	     :host github
    	     :repo "minad/consult"))


### Marginalia

    (use-package marginalia
      :straight (marginalia
    	     :host github
    	     :repo "minad/marginalia"
    	     :branch "main")
      :init
      (marginalia-mode +1)
      (cuss marginalia-annotators
    	(if (eq system-type 'windows-nt)
    	    '(marginalia-annotators-light
    	      marginalia-annotators-heavy)
    	  '(marginalia-annotators-heavy
    	    marginalia-annotators-light))))


## Ignore case

    (cuss completion-ignore-case t)
    (cuss read-buffer-completion-ignore-case t)
    (cuss read-file-name-completion-ignore-case t)


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


## Mouse


### Fix scrolling in margins

This is not *quite* correct yet.  For example, scrolling in the margins with a trackpad isn’t picked up (a trackpad sends different mouse events).

    (dolist (vec '([left-margin wheel-down]
    	       [right-margin wheel-down]
    	       [left-margin wheel-up]
    	       [right-margin wheel-up]))
      (bind-key vec #'mwheel-scroll))


## Keyboard


### Use `ESC` as a cancel key

From [link0ff](https://github.com/link0ff/emacs-init).  I thought they made a great point that `ESC` isn’t necessary to copy the `META` key on window-systems, which is where I use Emacs, anyway.

    (when window-system
      (define-key global-map [escape] 'keyboard-escape-quit)
      (define-key isearch-mode-map  [escape] 'isearch-cancel))


### Make `C-z` more useful as a prefix key

Also from link0ff.  See the above for a link.

    (defvar my-map
      (let ((map (make-sparse-keymap))
    	(c-z (global-key-binding "\C-z")))
        (global-unset-key "\C-z")
        (define-key global-map "\C-z" map)
        (define-key map "\C-z" c-z)
        map))
    (run-hooks 'my-map-defined-hook)


### Which-key

    (use-package which-key
      :config
      (which-key-mode +1))


### Bindings

1.  Switch to another window

        (bind-key "M-o" #'other-window)


# Persistence


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


## Save places in files

    (require 'saveplace)
    
    (cuss save-place-forget-unreadable-files
          (not (eq system-type 'windows-nt)))
    
    (save-place-mode 1)


## Recent files

    (require 'recentf)
    
    (cuss recentf-max-menu-items 100)
    (cuss recentf-max-saved-items 100)
    
    (with-eval-after-load 'no-littering
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory))
    
    (recentf-mode 1)


### Easily navigate recent files

    (defun recentf-find-file ()
      "Find a recent file using `completing-read'."
      (interactive)
      (let ((file (completing-read "Recent file: " recentf-list nil t)))
        (when file
          (find-file file))))
    
    (global-set-key (kbd "C-x C-r") #'recentf-find-file)


## Undo

    (use-package undo-fu
      :bind
      ("C-/" . undo-fu-only-undo)
      ("C-?" . undo-fu-only-redo))
    
    (use-package undo-fu-session
      :custom
      (undo-fu-session-incompatible-files
       '("/COMMIT_EDITMSG\\'"
         "/git-rebase-todo\\'"))
      :config
      (global-undo-fu-session-mode +1))


# Editing


## Operate visually on lines

    (global-visual-line-mode +1)


## Require a final newline

    (cuss require-final-newline t)


## Killing & Yanking


### Replace selection when typing

    (delete-selection-mode +1)


### Save existing clipboard text into kill ring before replacing it

    (cuss save-interprogram-paste-before-kill t)


### Sync the system clipboard and the kill ring

    (cuss yank-pop-change-selection t)


## So long mode

    (when (fboundp 'global-so-long-mode)
      (global-so-long-mode))


## Multiple cursors

    (use-package multiple-cursors
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . mc/mark-all-like-this))


## Expand region

    (use-package expand-region
      :bind
      (("C-=" . er/expand-region)
       ("C-+" . er/contract-region)))


## Highlight modified regions

    (use-package goggles
      :custom
      (goggles-pulse nil)
      :config
      (goggles-mode +1))


# Files


## Encoding


### UTF-8

    (set-language-environment "UTF-8")
    (set-terminal-coding-system 'utf-8)
    (cuss locale-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)


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


## Backups

    (cuss backup-by-copying 1)
    (cuss delete-old-versions -1)
    (cuss version-control t)
    (cuss vc-make-backup-files t)


## Auto-saves

    (auto-save-visited-mode 1)


## Revert files

    (cuss auto-revert-verbose nil)
    (global-auto-revert-mode +1)


## Add a timestamp to files

    (add-hook 'before-save-hook #'time-stamp)


# Programming


## Which function are we in?

    (which-function-mode +1)


## Parentheses


### Show parentheses

    (cuss show-paren-delay 0 "Show matching parens immediately.")
    (cuss show-paren-style 'mixed
          "Show parenthesis, or whole expression, depending on visibility.")
    (cuss show-paren-when-point-in-periphery t
          "Show paren when point is near-to paren.")
    (cuss show-paren-when-point-inside-paren t
          "Show surrounding parens.")
    
    (add-hook 'prog-mode-hook #'show-paren-mode)


### Smart parentheses

    (use-package smartparens
      :init
      (require 'smartparens-config)
      :config
      (show-smartparens-global-mode +1)
      :hook
      (prog-mode-hook . smartparens-strict-mode))


## Line numbers

    (defun acdw/enable-line-numbers ()
      "Enable line numbers, through either
      `display-line-numbers-mode' or through `linum-mode'."
      (if (and (fboundp 'display-line-numbers-mode)
    	   (display-graphic-p))
          (display-line-numbers-mode +1)
        (linum-mode +1)))
    
    (cuss display-line-numbers-width 2
          "Always have at least 2 digits for line numbers.")
    
    (add-hook 'prog-mode-hook #'acdw/enable-line-numbers)


## Indenting

    (use-package aggressive-indent
      :config
      (global-aggressive-indent-mode +1))


## Completion

    (use-package company
      :custom
      (company-idle-delay 0.1)
      (company-minimum-prefix-length 3)
    
      :init
      (defun acdw/company-complete-common-or-cycle+1 ()
        (interactive)
        (company-complete-common-or-cycle +1))
    
      (defun acdw/company-complete-common-or-cycle-1 ()
        (interactive)
        (company-complete-common-or-cycle -1))
    
      :bind
      (:map company-active-map
    	("C-n" . acdw/company-complete-common-or-cycle+1)
    	("C-p" . acdw/company-complete-common-or-cycle-1))
    
      :hook
      (prog-mode-hook . company-mode))
    
    (use-package company-prescient
      :hook
      (company-mode-hook . company-prescient-mode))
    
    ;; this comes with company-quickhelp, so....
    
    (use-package company-posframe
      :after (company)
      :config
      (company-posframe-mode +1))


## Languages


### Lua

    (use-package lua-mode
      :mode "\\.lua\\'"
      :interpreter "lua")


### Fennel

    (use-package fennel-mode
      :mode "\\.fnl\\'")


### Emacs lisp

    (cuss eval-expression-print-length nil
          "Don't truncate printed expressions by length.")
    (cuss eval-expression-print-level nil
          "Don't truncate printed expressions by level.")


# Writing


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
      (text-mode-hook . visual-fill-column-mode))


## Type nice-looking quote-type marks

    (use-package typo
      :hook
      (text-mode-hook . typo-mode))


## Insert *kaomoji*

    (use-package insert-kaomoji
      :bind
      ("C-x 8 k" . insert-kaomoji))


# Applications


## Magit

    (use-package magit
      :bind
      ("C-x g" . magit-status))


## Org mode

I’ve put org mode under Applications, as opposed to Writing, because it’s  more generally-applicable than that.

    (use-package org
      :mode ("\\.org\\'" . org-mode)
    
      :bind (:map org-mode-map
    	      ("M-n" . outline-next-visible-heading)
    	      ("M-p" . outline-previous-visible-heading))
    
      :custom
      (org-hide-emphasis-markers t)
      (org-fontify-done-headline t)
      (org-fontify-whole-heading-line t)
      (org-fontify-quote-and-verse-blocks t)
      (org-pretty-entities t)
      (org-num-mode +1)
    
      (cuss org-directory "~/Org")
    
      (org-src-tab-acts-natively t)
      (org-src-fontify-natively t)
      (org-src-window-setup 'current-window)
      (org-confirm-babel-evaluate nil)
    
      :config
      (require 'org-tempo)
      (require 'ox-md))


### Org Agenda

    (cuss org-agenda-files (no-littering-expand-etc-file-name "agenda-files"))
    
    (if (and (stringp org-agenda-files)
    	 (not (file-exists-p org-agenda-files)))
        (with-temp-buffer (write-file org-agenda-files)))


### Make bullets look like bullets

    (font-lock-add-keywords
     'org-mode
     '(("^ *\\([-+]\\) "
        (0 (prog1 ()
    	 (compose-region (match-beginning 1)
    			 (match-end 1)
    			 "•"))))))


### [A better return in Org mode](http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/)

    (require 'org-inlinetask)
    
    (defun scimax/org-return (&optional ignore)
      "Add new list item, heading or table row with RET.
    A double return on an empty element deletes it.
    Use a prefix arg to get regular RET."
      (interactive "P")
      (if ignore
          (org-return)
        (cond
    
         ((eq 'line-break (car (org-element-context)))
          (org-return t))
    
         ;; Open links like usual, unless point is at the end of a line.
         ;; and if at beginning of line, just press enter.
         ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
    	  (bolp))
          (org-return))
    
         ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
         ;; Johansson!
         ((org-inlinetask-in-task-p)
          (org-return))
    
         ;; checkboxes too
         ((org-at-item-checkbox-p)
          (org-insert-todo-heading nil))
    
         ;; lists end with two blank lines, so we need to make sure we are also not
         ;; at the beginning of a line to avoid a loop where a new entry gets
         ;; created with only one blank line.
         ((org-in-item-p)
          (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
    	  (org-insert-heading)
    	(beginning-of-line)
    	(delete-region (line-beginning-position) (line-end-position))
    	(org-return)))
    
         ;; org-heading
         ((org-at-heading-p)
          (if (not (string= "" (org-element-property :title (org-element-context))))
    	  (progn (org-end-of-meta-data)
    		 (org-insert-heading-respect-content)
    		 (outline-show-entry))
    	(beginning-of-line)
    	(setf (buffer-substring
    	       (line-beginning-position) (line-end-position)) "")))
    
         ;; tables
         ((org-at-table-p)
          (if (-any?
    	   (lambda (x) (not (string= "" x)))
    	   (nth
    	    (- (org-table-current-dline) 1)
    	    (org-table-to-lisp)))
    	  (org-return)
    	;; empty row
    	(beginning-of-line)
    	(setf (buffer-substring
    	       (line-beginning-position) (line-end-position)) "")
    	(org-return)))
    
         ;; fall-through case
         (t
          (org-return)))))
    
    
    (define-key org-mode-map (kbd "RET")
      'scimax/org-return)


### Insert blank lines

from [unpackaged.el](https://github.com/alphapapa/unpackaged.el#ensure-blank-lines-between-headings-and-before-contents).

    ;;;###autoload
    (defun unpackaged/org-fix-blank-lines (&optional prefix)
      "Ensure that blank lines exist between headings and between headings and their contents.
    With prefix, operate on whole buffer. Ensures that blank lines
    exist after each headings's drawers."
      (interactive "P")
      (org-map-entries (lambda ()
    		     (org-with-wide-buffer
    		      ;; `org-map-entries' narrows the buffer, which prevents us
    		      ;; from seeing newlines before the current heading, so we
    		      ;; do this part widened.
    		      (while (not (looking-back "\n\n" nil))
    			;; Insert blank lines before heading.
    			(insert "\n")))
    		     (let ((end (org-entry-end-position)))
    		       ;; Insert blank lines before entry content
    		       (forward-line)
    		       (while (and (org-at-planning-p)
    				   (< (point) (point-max)))
    			 ;; Skip planning lines
    			 (forward-line))
    		       (while (re-search-forward org-drawer-regexp end t)
    			 ;; Skip drawers. You might think that `org-at-drawer-p'
    			 ;; would suffice, but for some reason it doesn't work
    			 ;; correctly when operating on hidden text.  This
    			 ;; works, taken from `org-agenda-get-some-entry-text'.
    			 (re-search-forward "^[ \t]*:END:.*\n?" end t)
    			 (goto-char (match-end 0)))
    		       (unless (or (= (point) (point-max))
    				   (org-at-heading-p)
    				   (looking-at-p "\n"))
    			 (insert "\n"))))
    		   t (if prefix
    			 nil
    		       'tree)))

1.  Add a before-save-hook

        (defun cribbed/org-mode-fix-blank-lines ()
          (when (eq major-mode 'org-mode)
            (let ((current-prefix-arg 4)) ; Emulate C-u
              (call-interactively 'unpackaged/org-fix-blank-lines))))
        
        (add-hook 'before-save-hook #'cribbed/org-mode-fix-blank-lines)


## Elpher

    (use-package elpher
      :straight (elpher
    	     :repo "git://thelambdalab.xyz/elpher.git"
    	     :branch "patch_multiple_buffers")
    
      :custom
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
      (elpher-mode-hook . visual-fill-column-mode))


### Gemini mode

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
    
      :init
      (defun acdw/setup-gemini-mode ()
        (visual-fill-column-mode 1)
        (variable-pitch-mode -1))
    
      :hook
      (gemini-mode-hook . acdw/setup-gemini-mode))


### Gemini write

    (use-package gemini-write
      :straight (gemini-write
    	     :repo "https://alexschroeder.ch/cgit/gemini-write"))


### Ox-gemini

    (use-package ox-gemini
      :straight (ox-gemini
    	     :repo "https://git.sr.ht/~abrahms/ox-gemini"
    	     :branch "main"))


## Pastebin

    (use-package 0x0
      :custom
      (0x0-default-service 'ttm))


## RSS

    (use-package newsticker
      :custom
      (newsticker-url-list
       ;; LABEL URL [START-TIME] [INERVAL] [WGET-ARGUMENTS]
       '(("wsinatra" "http://lambdacreate.com/static/feed.rss")
         ("elioat" "https://eli.li/feed.rss")
         ("ACDW" "https://www.acdw.net/atom.xml")
         ("june" "https://text.causal.agency/feed.atom")
         ("kylie - notes" "https://www.somas.is/notes.atom")
         ("kylie - rhizome" "https://www.somas.is/rhizome.atom")
         ("brennan" "https://p1k3.com/all.xml")
         ("Planet Emacs" "https://planet.emacslife.com/atom.xml") 
         ("nullprogram, Chris Wellons" "https://nullprogram.com/feed/")
         ("Malleable Systems" "https://malleable.systems/blog/index.xml"))
       )
      :hook
      (newsticker-treeview-item-mode-hook . visual-fill-column-mode))


## Web browsing


### Open youtube links in mpv

from [karthinks](https://karthinks.com/software/more-batteries-included-with-emacs/#regexp-builder--m-x-re-builder).

    (require 'browse-url)
    
    (when (executable-find "mpv")
      (defun browse-url-mpv (url &optional single)
        (start-process "mpv" nil (if single "mpv" "umpv")
    		   (shell-quote-wildcard-pattern url)))
    
      (defun browse-url-at-point-mpv (&optional single)
        "Open a link in mpv."
        (interactive "P")
        (let ((browse-url-browser-function
    	   (if single
    	       (lambda
    		 (url &optional _new-window)
    		 (browse-url-mpv url t))
    	     #'browse-url-mpv)))
          (browse-url-at-point)))
    
      (cuss browse-url-browser-function
    	'(("https?:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
    	  ("." . browse-url-generic))))


## Reading e-books

    (use-package nov
      :mode ("\\.epub\\'" . nov-mode)
      :init
      (defun acdw/setup-nov-mode ()
        (visual-line-mode +1)
        (visual-fill-column-mode +1)
        (variable-pitch-mode +1))
      :config
      (cuss nov-text-width t)
      :hook
      (nov-mode-hook . acdw/setup-nov-mode))


## Eshell

    (when (executable-find "bash")
      (use-package bash-completion))
    
    (when (executable-find "fish")
      (use-package fish-completion
        :config
        (cuss fish-completion-fallback-on-bash-p (executable-find "bash"))
        (global-fish-completion-mode +1)))


# Appendices


## Emacs' files


### init.el

    ;; init.el -*- lexical-binding: t -*-

1.  Speed up init

        (setq gc-cons-threshold most-positive-fixnum)
        (defvar old-file-name-handler file-name-handler-alist)
        (setq file-name-handler-alist nil)

2.  Load config

    inspired by [Protesilaos Stavrou](https://protesilaos.com/dotemacs/#h:584c3604-55a1-49d0-9c31-abe46cb1f028).
    
        (let* ((conf (expand-file-name "config"
        			       user-emacs-directory))
               (conf-el (concat conf ".el"))
               (conf-org (concat conf ".org")))
          (unless (and (file-newer-than-file-p conf-el conf-org)
        	       (load conf 'no-error))
            (require 'org)
            (org-babel-load-file conf-org)))

3.  Reset for normal operation

        (setq gc-cons-threshold 16777216 ; 16mb
              gc-cons-percentage 0.1
              file-name-handler-alist old-file-name-handler)


### early-init.el

    ;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

    (setq load-prefer-newer t)
    (setq frame-inhibit-implied-resize t)


## Ease tangling and loading of Emacs' init

    (defun refresh-emacs (&optional disable-load)
      "Tangle `config.org', then byte-compile the resulting files.
    Then, load the byte-compilations unless passed with a prefix argument."
      (interactive "P")
      (let ((config (expand-file-name "config.org" user-emacs-directory)))
        (save-mark-and-excursion
          (with-current-buffer (find-file config)
    	(let ((prog-mode-hook nil))
    	  ;; generate the readme
    	  (when (file-newer-than-file-p config (expand-file-name
    						"README.md"
    						user-emacs-directory))
    	    (require 'ox-md)
    	    (org-md-export-to-markdown))
    	  ;; tangle config.org
    	  (when (file-newer-than-file-p config (expand-file-name
    						"config.el"
    						user-emacs-directory))
    	    (require 'org)
    	    (let ((inits (org-babel-tangle)))
    	      ;; byte-compile resulting files
    	      (dolist (f inits)
    		(when (string-match "\\.el\\'" f)
    		  (byte-compile-file f (not disable-load)))))))))))


### Add a hook to tangle when quitting

    (defun acdw/refresh-emacs-no-load ()
      (refresh-emacs 'disable-load))
    
    (add-hook 'kill-emacs-hook #'acdw/refresh-emacs-no-load)


## Ancillary scripts


### emacsdc

A wrapper script around emacs-client that starts the daemon if it hasn’t been yet.

    if ! emacsclient -nc "$@" 2>/dev/null; then
        emacs --daemon
        emacsclient -nc "$@"
    fi


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


### Note on the license

It's highly likely that the WTFPL is completely incompatible with the
GPL, for what should be fairly obvious reasons.  To that, I say:

**SUE ME, RMS!**

