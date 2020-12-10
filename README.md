
# Table of Contents

1.  [Pave the way](#org8865364)
    1.  [Correct `exec-path`](#orga7e2c6f)
    2.  [Package management](#org3148c95)
        1.  [Straight.el](#orgfaed4fc)
        2.  [Use-package](#org41bc53b)
        3.  [Extra use-package keywords](#org1849e90)
    3.  [Customize variables](#orgc528316)
        1.  [Put customizations in a separate file](#orgf585c3e)
        2.  [A macro for ease of customization](#orgad4e3a9)
    4.  [Keep a tidy `~/.emacs`](#orgf4003c0)
2.  [Look and Feel](#org11edc2b)
    1.  [Simplify the UI](#orgb076fea)
        1.  [Tool bars and menu bars](#org586efe9)
        2.  [Scroll bars](#org298e07a)
        3.  [Dialog boxen](#org439037b)
        4.  [Shorten confirmations](#orgbb289dc)
        5.  [Remove the bell](#org2d64864)
        6.  [Tell Ediff to setup windows better](#org0b4a638)
    2.  [Tweak the remaining UI](#org2ca3449)
        1.  [Fringes](#orgae020e2)
        2.  [Minibuffer](#orgc8f4148)
        3.  [Tabs](#org83e5b37)
        4.  [Cursor](#org818f06f)
        5.  [Buffer names](#org403cbfb)
        6.  [Buffer boundaries](#org86d72cc)
    3.  [Startup](#org06a8610)
    4.  [Theme](#org3cecd0a)
        1.  [Modeline](#org1cecd04)
        2.  [Fonts](#org52f810d)
3.  [Interactivity](#orgb1b190a)
    1.  [Selectrum](#orge91bc0a)
    2.  [Prescient](#org2dec02d)
    3.  [Consult](#org793390e)
    4.  [Ignore case](#orgf0e1425)
    5.  [Search](#orgcb1ee5e)
    6.  [Mouse](#orgf588a26)
4.  [Persistence](#orgb6ade5d)
    1.  [Save history](#org9a6f451)
    2.  [Save places in files](#org3041895)
    3.  [Recent files](#orgfddf139)
        1.  [Easily navigate recent files](#org5211a84)
    4.  [Undo](#org203b48b)
5.  [Editing](#org697ec47)
    1.  [Operate visually on lines](#org958bd79)
    2.  [Require a final newline](#orgc5ef3b6)
    3.  [Killing & Yanking](#orgba62063)
        1.  [Replace selection when typing](#orgcb04f02)
        2.  [Save existing clipboard text into kill ring before replacing it](#orgdf80771)
    4.  [So long mode](#org2252a8b)
    5.  [Multiple cursors](#org6fe3b6b)
    6.  [Expand region](#orgfe5b963)
6.  [Files](#org0b92a32)
    1.  [Encoding](#orgf1babbf)
        1.  [UTF-8](#org8dd350d)
        2.  [Convert all files to UNIX-style line endings](#org34854a5)
    2.  [Backups](#org184d029)
    3.  [Auto-saves](#org72595b4)
    4.  [Revert files](#orgd374486)
    5.  [Add a timestamp to files](#org8ac0e64)
7.  [Programming](#org97bb90c)
    1.  [Which function are we in?](#orgbcce753)
    2.  [Parentheses](#org7d7c4d6)
        1.  [Show parentheses](#orgbe3915d)
        2.  [Smart parentheses](#org9415ceb)
    3.  [Line numbers](#org2ebc45e)
8.  [Writing](#orge8c66c9)
    1.  [Visual Fill Column](#org5278827)
    2.  [Type nice-looking quote-type marks](#org63bd7b9)
9.  [Applications](#org50abb5d)
    1.  [Magit](#orgf68bfba)
    2.  [Org mode](#org66c3b6b)
        1.  [Export to markdown](#org34de14b)
        2.  [Make bullets look like bullets](#orge32cf6b)
        3.  [A better return in Org mode](#org5d13ece)
10. [Appendices](#org1c0423c)
    1.  [Emacs' files](#orgea74573)
        1.  [init.el](#org1ed6b18)
        2.  [early-init.el](#org1e22a5e)
    2.  [Ease tangling and loading of Emacs' init](#org50bd83d)
    3.  [License](#org551e420)
        1.  [Note on the license](#org15f94a8)
    4.  [Keymaps for *this* file](#org3bda9ee)



<a id="org8865364"></a>

# Pave the way


<a id="orga7e2c6f"></a>

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


<a id="org3148c95"></a>

## Package management


<a id="orgfaed4fc"></a>

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


<a id="org41bc53b"></a>

### Use-package

    (setq straight-use-package-by-default t)
    (setq use-package-hook-name-suffix nil)
    (straight-use-package 'use-package)


<a id="org1849e90"></a>

### Extra use-package keywords

1.  :custom-update

        (straight-use-package
         '(use-package-custom-update
           :host github
           :repo "a13/use-package-custom-update"))
        
        (require 'use-package-custom-update)


<a id="orgc528316"></a>

## Customize variables


<a id="orgf585c3e"></a>

### Put customizations in a separate file

    (setq custom-file
          (expand-file-name "custom.el" user-emacs-directory))


<a id="orgad4e3a9"></a>

### A macro for ease of customization

    (defmacro cuss (var val &optional docstring)
      "Basically `:custom' from `use-package', broken out."
      `(funcall (or (get ',var 'custom-set) #'set-default)
    	    ',var ,val))


<a id="orgf4003c0"></a>

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


<a id="org11edc2b"></a>

# Look and Feel


<a id="orgb076fea"></a>

## Simplify the UI


<a id="org586efe9"></a>

### Tool bars and menu bars

    (cuss default-frame-alist
          '((tool-bar-lines . 0)
    	(menu-bar-lines . 0)))
    
    (menu-bar-mode -1)
    (tool-bar-mode -1)


<a id="org298e07a"></a>

### Scroll bars

    (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
    (scroll-bar-mode -1)
    
    (add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
    (horizontal-scroll-bar-mode -1)


<a id="org439037b"></a>

### Dialog boxen

    (cuss use-dialog-box nil)


<a id="orgbb289dc"></a>

### Shorten confirmations

    (fset 'yes-or-no-p #'y-or-n-p)


<a id="org2d64864"></a>

### Remove the bell

    (cuss visible-bell (not (string= (system-name) "larry")))


<a id="org0b4a638"></a>

### Tell Ediff to setup windows better

    (declare-function ediff-setup-windows-plain "ediff-wind.el")
    (cuss ediff-window-setup-function #'ediff-setup-windows-plain)


<a id="org2ca3449"></a>

## Tweak the remaining UI


<a id="orgae020e2"></a>

### Fringes

    (add-to-list 'default-frame-alist '(left-fringe-width . 2))
    (add-to-list 'default-frame-alist '(right-fringe-width . 2))


<a id="orgc8f4148"></a>

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


<a id="org83e5b37"></a>

### Tabs

1.  Show the tabs as current buffer, plus window count

        (cuss tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)

2.  Only show the tab bar when there's more than one tab

        (cuss tab-bar-show 1
              "Show the tab bar only when there's more than 1 tab.")


<a id="org818f06f"></a>

### Cursor

    (cuss cursor-type 'bar
          "Show a vertical bar for the cursor.")
    (cuss cursor-in-non-selected-windows 'hollow
          "In inactive windows, make the cursor an empty box.")
    (blink-cursor-mode 0)


<a id="org403cbfb"></a>

### Buffer names

    (require 'uniquify)
    (cuss uniquify-buffer-name-style 'forward)


<a id="org86d72cc"></a>

### Buffer boundaries

    (cuss indicate-buffer-boundaries
          '((top . right)
    	(bottom . right)
    	(t . nil)))
    
    (cuss indicate-empty-lines t)


<a id="org06a8610"></a>

## Startup

    (cuss inhibit-startup-screen t "Don't show Emacs' startup buffer.")
    (cuss initial-buffer-choice t "Start at *scratch*.")
    (cuss initial-scratch-message "" "Empty *scratch*.")


<a id="org3cecd0a"></a>

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


<a id="org1cecd04"></a>

### Modeline

    (custom-set-faces
     '(mode-line ((t (:family "fixed"
    			  :height 100
    			  :overline t
    			  :box nil
    			  :foreground "black"
    			  :background "white"))))
     '(mode-line-inactive ((t (:family "fixed"
    				   :height 80
    				   :overline t
    				   :box nil
    				   :foreground "#808080"
    				   :background "white")))))


<a id="org52f810d"></a>

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
          (when (display-graphic-p)
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
        
            (remove-hook 'window-setup-hook #'acdw/setup-fonts)))
        
        (add-hook 'window-setup-hook #'acdw/setup-fonts)

2.  Variable-pitch in text modes

        (add-hook 'text-mode-hook #'variable-pitch-mode)

3.  Line spacing

        (cuss line-spacing 0.1)

4.  Unicode fonts

        (use-package unicode-fonts
          :config
          (unicode-fonts-setup))


<a id="orgb1b190a"></a>

# Interactivity


<a id="orge91bc0a"></a>

## Selectrum

    (use-package selectrum
      :config
      (selectrum-mode +1))


<a id="org2dec02d"></a>

## Prescient

    (use-package prescient
      :config
      (prescient-persist-mode +1))
    
    (use-package selectrum-prescient
      :after (selectrum prescient)
      :config
      (selectrum-prescient-mode +1))


<a id="org793390e"></a>

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


<a id="orgf0e1425"></a>

## Ignore case

    (cuss completion-ignore-case t)
    (cuss read-buffer-completion-ignore-case t)
    (cuss read-file-name-completion-ignore-case t)


<a id="orgcb1ee5e"></a>

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


<a id="orgf588a26"></a>

## Mouse

    (dolist (vec '([left-margin wheel-down]
    	       [right-margin wheel-down]
    	       [left-margin wheel-up]
    	       [right-margin wheel-up]))
      (bind-key vec #'mwheel-scroll))


<a id="orgb6ade5d"></a>

# Persistence


<a id="org9a6f451"></a>

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


<a id="org3041895"></a>

## Save places in files

    (require 'saveplace)
    
    (cuss save-place-forget-unreadable-files
          (not (eq system-type 'windows-nt)))
    
    (save-place-mode 1)


<a id="orgfddf139"></a>

## Recent files

    (require 'recentf)
    
    (cuss recentf-max-menu-items 100)
    (cuss recentf-max-saved-items 100)
    
    (with-eval-after-load 'no-littering
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory))
    
    (recentf-mode 1)


<a id="org5211a84"></a>

### Easily navigate recent files

    (defun recentf-find-file ()
      "Find a recent file using `completing-read'."
      (interactive)
      (let ((file (completing-read "Recent file: " recentf-list nil t)))
        (when file
          (find-file file))))
    
    (global-set-key (kbd "C-x C-r") #'recentf-find-file)


<a id="org203b48b"></a>

## Undo

    (use-package undohist
      :config
      (undohist-initialize))


<a id="org697ec47"></a>

# Editing


<a id="org958bd79"></a>

## Operate visually on lines

    (global-visual-line-mode +1)


<a id="orgc5ef3b6"></a>

## Require a final newline

    (cuss require-final-newline t)


<a id="orgba62063"></a>

## Killing & Yanking


<a id="orgcb04f02"></a>

### Replace selection when typing

    (delete-selection-mode +1)


<a id="orgdf80771"></a>

### Save existing clipboard text into kill ring before replacing it

    (cuss save-interprogram-paste-before-kill t)


<a id="org2252a8b"></a>

## So long mode

    (when (fboundp 'global-so-long-mode)
      (global-so-long-mode))


<a id="org6fe3b6b"></a>

## Multiple cursors

    (use-package multiple-cursors
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . mc/mark-all-like-this))


<a id="orgfe5b963"></a>

## Expand region

    (use-package expand-region
      :bind
      (("C-=" . er/expand-region)
       ("C-+" . er/contract-region)))


<a id="org0b92a32"></a>

# Files


<a id="orgf1babbf"></a>

## Encoding


<a id="org8dd350d"></a>

### UTF-8

    (set-language-environment 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (cuss locale-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)


<a id="org34854a5"></a>

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


<a id="org184d029"></a>

## Backups

    (cuss backup-by-copying 1)
    (cuss delete-old-versions -1)
    (cuss version-control t)
    (cuss vc-make-backup-files t)


<a id="org72595b4"></a>

## Auto-saves

    (auto-save-visited-mode 1)


<a id="orgd374486"></a>

## Revert files

    (cuss auto-revert-verbose nil)
    (global-auto-revert-mode +1)


<a id="org8ac0e64"></a>

## Add a timestamp to files

    (add-hook 'before-save-hook #'time-stamp)


<a id="org97bb90c"></a>

# Programming


<a id="orgbcce753"></a>

## Which function are we in?

    (which-function-mode +1)


<a id="org7d7c4d6"></a>

## Parentheses


<a id="orgbe3915d"></a>

### Show parentheses

    (cuss show-paren-delay 0 "Show matching parens immediately.")
    (cuss show-paren-style 'mixed
          "Show parenthesis, or whole expression, depending on visibility.")
    (cuss show-paren-when-point-in-periphery t
          "Show paren when point is near-to paren.")
    (cuss show-paren-when-point-inside-paren t
          "Show surrounding parens.")
    
    (add-hook 'prog-mode-hook #'show-paren-mode)


<a id="org9415ceb"></a>

### Smart parentheses

    (use-package smartparens
      :init
      (require 'smartparens-config)
      :config
      (show-smartparens-global-mode +1)
      :hook
      (prog-mode-hook . smart-parens-strict-mode))


<a id="org2ebc45e"></a>

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


<a id="orge8c66c9"></a>

# Writing


<a id="org5278827"></a>

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


<a id="org63bd7b9"></a>

## Type nice-looking quote-type marks

    (use-package typo
      :hook
      (text-mode-hook . typo-mode))


<a id="org50abb5d"></a>

# Applications


<a id="orgf68bfba"></a>

## Magit

    (use-package magit
      :bind
      ("C-x g" . magit-status))


<a id="org66c3b6b"></a>

## Org mode

I’ve put org mode under Applications, as opposed to [8](#orge8c66c9), because it’s  more generally-applicable than that.

    (use-package org
      :custom
      (org-hide-emphasis-markers t)
      (org-fontify-done-headline t)
      (org-fontify-whole-heading-line t)
      (org-fontify-quote-and-verse-blocks t)
      (org-pretty-entities t)
    
      (org-src-tab-acts-natively t)
      (org-src-fontify-natively t)
      (org-src-window-setup 'current-window)
      (org-confirm-babel-evaluate nil))


<a id="org34de14b"></a>

### Export to markdown

    (require 'ox-md)


<a id="orge32cf6b"></a>

### Make bullets look like bullets

    (font-lock-add-keywords
     'org-mode
     '(("^ *\\([-+]\\) "
        (0 (prog1 ()
    	 (compose-region (match-beginning 1)
    			 (match-end 1)
    			 "•"))))))


<a id="org5d13ece"></a>

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


<a id="org1c0423c"></a>

# Appendices


<a id="orgea74573"></a>

## Emacs' files


<a id="org1ed6b18"></a>

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


<a id="org1e22a5e"></a>

### early-init.el

    ;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

    (setq load-prefer-newer t)
    (setq frame-inhibit-implied-resize t)


<a id="org50bd83d"></a>

## Ease tangling and loading of Emacs' init

    (defun refresh-emacs (&optional disable-load)
      (interactive "P")
      "Tangle `config.org', then byte-compile the resulting files.
    Then, load the byte-compilations unless passed with a prefix argument."
      (let ((config (expand-file-name "config.org" user-emacs-directory)))
        (save-mark-and-excursion
          (with-current-buffer (find-file config)
           (let ((prog-mode-hook nil))
    	 ;; generate the readme
    	 (require 'ox-md)
    	 (org-md-export-to-markdown)
    	 ;; tangle config.org
    	 (require 'org)
    	 (let ((inits (org-babel-tangle)))
    	   ;; byte-compile resulting files
    	   (dolist (f inits)
    	     (when (string-match "\\.el\\'" f)
    	       (byte-compile-file f (not disable-load))))))))))


<a id="org551e420"></a>

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


<a id="org15f94a8"></a>

### Note on the license

It's highly likely that the WTFPL is completely incompatible with the
GPL, for what should be fairly obvious reasons.  To that, I say:

**SUE ME, RMS!**


<a id="org3bda9ee"></a>

## Keymaps for *this* file

This isn’t working … yet.

