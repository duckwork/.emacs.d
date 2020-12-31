Why the hell not, let’s do this again.


# Basics


## About me

    (setq user-full-name "Case Duckworth"
          user-mail-address "acdw@acdw.net")


## Correct `exec-path`

Straight depends on Git, so I need to tell Emacs where different paths are.

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

Straight can't bootstrap itself on Windows, so I've wrapped the
bootstrap code from straight's repo in a function.

    (defun acdw/bootstrap-straight ()
      "Bootstrap straight.el."
      (defvar bootstrap-version)
      (let ((bootstrap-file
    	 (expand-file-name
    	  "straight/repos/straight.el/bootstrap.el"
    	  user-emacs-directory))
    	(bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
    	  (url-retrieve-synchronously
    	   (concat
    	    "https://raw.githubusercontent.com/"
    	    "raxod502/straight.el/"
    	    "develop/install.el")
    	   'silent 'inhibit-cookies)
    	(goto-char (point-max))
    	(eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage)))

Now, I'll *try* running it regular-style, ignoring the errors.  If it
doesn't work, I'll call git directly and clone the repo myself.

    (unless (ignore-errors (acdw/bootstrap-straight))
      (message "Straight.el didn't bootstrap correctly.  Cloning directly...")
      (call-process "git" nil
    		(get-buffer-create "*bootstrap-straight-messages*") nil
    		"clone"
    		"https://github.com/raxod502/straight.el"
    		(expand-file-name "straight/repos/straight.el"
    				  user-emacs-directory))
      (acdw/bootstrap-straight))


## Customize macro

    (defmacro cuss (var val &optional docstring)
      "Basically, `:custom' from `use-package', but without `use-package'."
      (declare (doc-string 3)
    	   (indent 2))
      `(funcall (or (get ',var 'custom-set) #'set-default)
    	    ',var ,val))


## Clean `.emacs.d`

    (straight-use-package 'no-littering)
    (require 'no-littering)


## Look and feel


### Fonts

1.  Define fonts

        (defun set-face-from-alternatives (face fonts)
          (catch :return
            (dolist (font fonts)
              (when (find-font (font-spec :family (car font)))
            (apply #'set-face-attribute `(,face nil
        			:family ,(car font)
        			,@(cdr font)))
            (throw :return font)))))
        
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
        		   :height 110)
        		  ("Linux Libertine Mono O"
        		   :height 110)
        		  ("Go Mono"
        		   :height 100)
        		  ("Consolas"
        		   :height 100)))
        
            (set-face-from-alternatives 'variable-pitch
        		'(("Libertinus Serif"
        		   :height 120)
        		  ("Linux Libertine O"
        		   :height 120)
        		  ("Georgia"
        		   :height 110)))
        
            (remove-function after-focus-change-function #'acdw/setup-fonts)))
        
        (add-function :before after-focus-change-function #'acdw/setup-fonts)

2.  Line spacing

        (cuss line-spacing 0.1
          "Add 10% extra space below each line.")

3.  Unicode Fonts

        (straight-use-package 'unicode-fonts)
        (require 'unicode-fonts)
        (unicode-fonts-setup)


### Cursor

    (cuss cursor-type 'bar
      "Show a vertical bar for the cursor.")
    
    (cuss cursor-in-non-selected-windows 'hollow
      "Show an empty box in inactive windows.")
    
    ;; Don't blink the cursor
    (blink-cursor-mode -1)


### Tool Bars

1.  Tool bars and menu bars

        (cuss default-frame-alist
              '((tool-bar-lines . 0)
        	(menu-bar-lines .0))
          "Setup the default frame alist.")
        
        (menu-bar-mode -1)
        (tool-bar-mode -1)

2.  Scroll bars

        (add-to-list 'default-frame-alist
        	     '(vertical-scroll-bars . nil))
        
        (scroll-bar-mode -1)
        
        (add-to-list 'default-frame-alist
        	     '(horizontal-scroll-bars . nil))
        
        (horizontal-scroll-bar-mode -1)


### Dialogs

    (cuss use-dialog-box nil
      "Don't use dialog boxes to ask questions.")

1.  Yes or no questions

        (fset 'yes-or-no-p #'y-or-n-p)

2.  The Bell

        (defun acdw/ring-bell-function ()
          "Ring the bell."
          (let ((orig-face (face-foreground 'mode-line)))
            (set-face-foreground 'mode-line "#F2804F")
            (run-with-idle-timer
             0.1 nil
             (lambda (fg)
               (set-face-foreground 'mode-line fg))
             orig-face)))
        
        (cuss ring-bell-function #'acdw/ring-bell-function)


### Frames

1.  Fringes

        (cuss indicate-empty-lines t
          "Show an indicator on the left fringe of empty lines past the
        end of the buffer.")
        (cuss indicate-buffer-boundaries 'right
          "Indicate the beginning and end of the buffer and whether it
          scrolls off-window in the right fringe.")

2.  Minibuffer

        (cuss minibuffer-prompt-properties
            '(read-only t cursor-intangible t face minibuffer-prompt)
          "Keep the cursor away from the minibuffer prompt.")

3.  Tabs

        (cuss tab-bar-tab-name-function
            #'tab-bar-tab-name-current-with-count
          "Show the tab name as the name of the current buffer, plus a
          count of the windows in the tab.")
        
        (cuss tab-bar-show 1
          "Show the tab bar, when there's more than one tab.")


### Windows

1.  Winner mode

        (when (fboundp 'winner-mode)
          (winner-mode +1))

2.  Switch windows

        (global-set-key (kbd "M-o") #'other-window)


### Buffers

1.  Uniquify buffers

        (require 'uniquify)
        (cuss uniquify-buffer-name-style 'forward
          "Uniquify buffers' names by going up the path trees until they
        become unique.")

2.  Startup buffers

        (cuss inhibit-startup-screen t
          "Don't show Emacs' startup buffer.")
        
        (cuss initial-buffer-choice t
          "Start with *scratch*.")
        
        (cuss initial-scratch-message ""
          "Empty *scratch* buffer.")


### Modeline

1.  Smart mode line

        (straight-use-package 'smart-mode-line)
        
        (cuss sml/no-confirm-load-theme t
          "Pass the NO-CONFIRM flag to `load-theme'.")
        
        (sml/setup)

2.  Rich minority

    Since this *comes* with smart mode line, I’m just going to use it,
    instead of `diminish` or another package.  I do have to write this
    helper function, though, to add things to the whitelist.
    
        (defun rm/whitelist-add (regexp)
          "Add a REGEXP to the whitelist for `rich-minority'."
          (if (listp 'rm--whitelist-regexps)
              (add-to-list 'rm--whitelist-regexps regexp)
            (setq rm--whitelist-regexps `(,regexp)))
          (setq rm-whitelist
        	(mapconcat 'identity rm--whitelist-regexps "\\|")))
        
        (straight-use-package 'rich-minority)
        
        (rm/whitelist-add "^$")


### Theme

1.  Modus Themes

        (straight-use-package 'modus-themes)
        
        (cuss modus-themes-slanted-constructs t
          "Use more slanted constructs.")
        (cuss modus-themes-bold-constructs t
          "Use more bold constructs.")
        
        (cuss modus-themes-region 'bg-only
          "Only highlight the background of the selected region.")
        
        (cuss modus-themes-org-blocks 'grayscale
          "Show org-blocks with a grayscale background.")
        (cuss modus-themes-headings
            '((1 . line)
              (t . t))
          "Highlight top headings with `line' style, and others by default.")
        
        (cuss modus-themes-scale-headings t
          "Scale headings by the ratios below.")
        (cuss modus-themes-scale-1 1.1)
        (cuss modus-themes-scale-2 1.15)
        (cuss modus-themes-scale-3 1.21)
        (cuss modus-themes-scale-4 1.27)
        (cuss modus-themes-scale-5 1.33)
        
        (load-theme 'modus-operandi t)

2.  Change themes based on time of day

        (cuss calendar-latitude 30.4515)
        (cuss calendar-longitude -91.1871)
        
        (straight-use-package 'circadian)
        
        (cuss circadian-themes '((:sunrise . modus-operandi)
        			 (:sunset . modus-vivendi)))
        
        (circadian-setup)


## Interactivity


### Completing read

1.  Shadow file names in `completing-read`.

        (cuss file-name-shadow-properties '(invisible t))
        
        (file-name-shadow-mode +1)

2.  Ignore case in `completing-read`

        (cuss completion-ignore-case t)
        (cuss read-buffer-completion-ignore-case t)
        (cuss read-file-name-completion-ignore-case t)

3.  Selectrum

        (straight-use-package 'selectrum)
        (require 'selectrum)
        (selectrum-mode +1)

4.  Prescient

        (straight-use-package 'prescient)
        (require 'prescient)
        (prescient-persist-mode +1)
        
        (straight-use-package 'selectrum-prescient)
        (require 'selectrum-prescient)
        (selectrum-prescient-mode +1)

5.  Consult

        (straight-use-package '(consult
        			:host github
        			:repo "minad/consult"))
        (require 'consult)
        
        (straight-use-package '(consult-selectrum
        			:host github
        			:repo "minad/consult"))
        (require 'consult-selectrum)
        
        (define-key ctl-x-map "b" #'consult-buffer)
        (define-key ctl-x-map (kbd "C-r") #'consult-buffer)
        (define-key ctl-x-map "4b" #'consult-buffer-other-window)
        (define-key ctl-x-map "5b" #'consult-buffer-other-frame)
        
        (define-key goto-map "o" #'consult-outline)
        (define-key goto-map "g" #'consult-line)
        (define-key goto-map (kbd "M-g") #'consult-line)
        (define-key goto-map "l" #'consult-line)
        (define-key goto-map "m" #'consult-mark)
        (define-key goto-map "k" #'consult-global-mark)
        (define-key goto-map "i" #'consult-imenu)
        (define-key goto-map "e" #'consult-error)
        
        (global-set-key (kbd "M-y") #'consult-yank-pop)
        
        (define-key help-map "a" #'consult-apropos)
        
        (fset 'multi-occur #'consult-multi-occur)

6.  Marginalia

        (straight-use-package '(marginalia
        			:host github
        			:repo "minad/marginalia"
        			:branch "main"))
        
        (cuss marginalia-annotators
            '(marginalia-annotators-heavy
              marginalia-annotators-light))
        
        (marginalia-mode +1)


## Keyboard


### `ESC` cancels all

    (global-set-key (kbd "<escape>") #'keyboard-escape-quit)


### Personal prefix key: `C-z`

    (defvar acdw/map
      (let ((map (make-sparse-keymap))
    	(c-z (global-key-binding "\C-z")))
        (global-unset-key "\C-z")
        (define-key global-map "\C-z" map)
        (define-key map "\C-z" c-z)
        map))
    
    (run-hooks 'acdw/map-defined-hook)


## Persistence


### Minibuffer history

    (require 'savehist)
    
    (cuss savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring)
      "Other variables to save alongside the minibuffer history.")
    
    (cuss history-length t
      "Don't truncate history.")
    
    (cuss history-delete-duplicates t
      "Delete history duplicates.")
    
    (savehist-mode +1)


### File places

    (require 'saveplace) ; this isn't required, but ... I like having it here
    
    (cuss save-place-forget-unreadable-files t
      "Don't check if files are readable or not.")
    
    (save-place-mode +1)


### Recent files

    (require 'recentf)
    
    (cuss recentf-max-menu-items 100
      "The maximum number of items in the recentf menu.")
    (cuss recentf-max-saved-items nil
      "Don't limit the number of recent files.")
    
    (with-eval-after-load 'no-littering
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory))
    
    (recentf-mode +1)
    
    ;; save the recentf-list every 5 minutes
    (run-at-time nil (* 5 60) 'recentf-save-list)


## Undo

    (straight-use-package 'undo-fu)
    (require 'undo-fu)
    
    (global-set-key (kbd "C-/") #'undo-fu-only-undo)
    (global-set-key (kbd "C-?") #'undo-fu-only-redo)
    
    (straight-use-package 'undo-fu-session)
    (require 'undo-fu-session)
    
    (cuss undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'"
          "/git-rebase-todo\\'")
      "A list of files that are incompatible with the concept of undo sessions.")
    
    (with-eval-after-load 'no-littering
      (let ((dir (no-littering-expand-var-file-name "undos")))
        (make-directory dir 'parents)
        (cuss undo-fu-session-directory dir)))
    
    (global-undo-fu-session-mode +1)


## Files


### Encoding

1.  UTF-8

        (set-language-environment "UTF-8")
        (set-terminal-coding-system 'utf-8)
        (cuss locale-coding-system 'utf-8)
        (set-default-coding-systems 'utf-8)
        (set-selection-coding-system 'utf-8)
        (prefer-coding-system 'utf-8)

2.  Convert all files to UNIX-style line endings

    from [Emacs Wiki](https://www.emacswiki.org/emacs/EndOfLineTips).
    
        (defun ewiki/no-junk-please-were-unixish ()
          "Convert line endings to UNIX, dammit."
          (let ((coding-str (symbol-name buffer-file-coding-system)))
            (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
              (set-buffer-file-coding-system 'unix))))
    
    I add it to the `find-file-hook` *and* `before-save-hook` because I
    don't want to ever work with anything other than UNIX line endings
    ever again.  I just don't care.  Even Microsoft Notepad can handle
    UNIX line endings, so I don't want to hear it.
    
        (add-hook 'find-file-hook #'ewiki/no-junk-please-were-unixish)
        (add-hook 'before-save-hook #'ewiki/no-junk-please-were-unixish)


### Backups

    (cuss backup-by-copying 1)
    (cuss delete-old-versions -1)
    (cuss version-control t)
    (cuss vc-make-backup-files t)
    
    (with-eval-after-load 'no-littering
      (let ((dir (no-littering-expand-var-file-name "backup")))
        (make-directory dir 'parents)
        (cuss backup-directory-alist
    	`((".*" . ,dir)))))


### Auto-saves

    (with-eval-after-load 'no-littering
      (let ((dir (no-littering-expand-var-file-name "autosaves")))
        (make-directory dir 'parents)
        (cuss auto-save-file-name-transforms
    	`((".*" ,dir t))))
    
      (auto-save-visited-mode +1))


### Auto-revert buffers to files on disk

    (global-auto-revert-mode +1)


### Add a timestamp to files

    (add-hook 'before-save-hook #'time-stamp)


### Require a final new line

    (cuss require-final-newline t)


## Text editing


### Operate visually on lines

    (global-visual-line-mode +1)


### Stay snappy with long-lined files

    (when (fboundp 'global-so-long-mode)
      (global-so-long-mode +1))


### Killing & Yanking

1.  Replace selection when typing

        (delete-selection-mode +1)

2.  Work better with the system clipboard

        (cuss save-interprogram-paste-before-kill t
          "Save existing clipboard text into the kill ring before
          replacing it.")
        
        (cuss yank-pop-change-selection t
          "Update the X selection when rotating the kill ring.")


### Searching & Replacing

    (straight-use-package 'visual-regexp)
    (require 'visual-regexp)
    
    (with-eval-after-load 'visual-regexp
      (global-set-key (kbd "M-C-%") #'vr/query-replace))


# Programming


## Parentheses


### Smart parentheses

    (straight-use-package 'smartparens)
    (require 'smartparens-config)
    
    ;; replace show-paren
    
    (cuss sp-show-pair-delay 0
      "Don't delay before showing the pairs.")
    (cuss sp-show-pair-from-inside t
      "Highlight the enclosing pair when immediately inside.")
    
    (add-hook 'prog-mode-hook #'show-smartparens-mode +1)
    
    ;; enable strict smartparens in prog mode
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)


## Indent aggressively

    (straight-use-package 'aggressive-indent)
    
    (global-aggressive-indent-mode +1)


## Language-specific packages


### Emacs lisp

    (cuss eval-expression-print-length nil
      "Don't truncate printed expressions by length.")
    (cuss eval-expression-print-level nil
      "Don't truncate printed expressions by level.")


# Writing


## Visual fill column


### Fix scrolling in margins

This has to be done *before* loading the package.  It's included in `visual-fill-column`, too, but for some reason isn't loaded there.

    (global-set-key [right-margin mouse-1] (global-key-binding [mouse-1])) ; #'mouse-set-point
    (global-set-key [right-margin mouse-2] (global-key-binding [mouse-2])) ; #'mouse-yank-primary
    (global-set-key [right-margin mouse-3] (global-key-binding [mouse-3])) ; #'mouse-save-then-kill
    (global-set-key [right-margin drag-mouse-1] #'ignore)
    (global-set-key [right-margin drag-mouse-2] #'ignore)
    (global-set-key [right-margin drag-mouse-3] #'ignore)
    (global-set-key [right-margin double-mouse-1] #'ignore)
    (global-set-key [right-margin double-mouse-2] #'ignore)
    (global-set-key [right-margin double-mouse-3] #'ignore)
    (global-set-key [right-margin triple-mouse-1] #'ignore)
    (global-set-key [right-margin triple-mouse-2] #'ignore)
    (global-set-key [right-margin triple-mouse-3] #'ignore)
    (global-set-key [left-margin mouse-1] (global-key-binding [mouse-1])) ; #'mouse-set-point
    (global-set-key [left-margin mouse-2] (global-key-binding [mouse-2])) ; #'mouse-yank-primary
    (global-set-key [left-margin mouse-3] (global-key-binding [mouse-3])) ; #'mouse-save-then-kill
    (global-set-key [left-margin drag-mouse-1] #'ignore)
    (global-set-key [left-margin drag-mouse-2] #'ignore)
    (global-set-key [left-margin drag-mouse-3] #'ignore)
    (global-set-key [left-margin double-mouse-1] #'ignore)
    (global-set-key [left-margin double-mouse-2] #'ignore)
    (global-set-key [left-margin double-mouse-3] #'ignore)
    (global-set-key [left-margin triple-mouse-1] #'ignore)
    (global-set-key [left-margin triple-mouse-2] #'ignore)
    (global-set-key [left-margin triple-mouse-3] #'ignore)
    
    (mouse-wheel-mode +1)
    
    (when (bound-and-true-p mouse-wheel-mode)
      (global-set-key [right-margin mouse-wheel-down-event] #'mwheel-scroll)
      (global-set-key [right-margin mouse-wheel-up-event] #'mwheel-scroll)
      (global-set-key [right-margin wheel-down] #'mwheel-scroll)
      (global-set-key [right-margin wheel-up] #'mwheel-scroll)
      (global-set-key [left-margin mouse-wheel-down-event] #'mwheel-scroll)
      (global-set-key [left-margin mouse-wheel-up-event] #'mwheel-scroll)
      (global-set-key [left-margin wheel-down] #'mwheel-scroll)
      (global-set-key [left-margin wheel-up] #'mwheel-scroll))


### Load the package

    (straight-use-package 'visual-fill-column)
    
    (cuss visual-fill-column-center-text nil
      "Whether to center the text in the frame.")
    
    (cuss fill-column 84
      "Width of fill-column, and thus, visual-fill-column.")
    
    (advice-add 'text-scale-adjust
    	    :after #'visual-fill-column-adjust)
    
    (global-visual-fill-column-mode +1)


## Typographical niceties


### Typo mode

    (straight-use-package 'typo)
    
    (add-hook 'text-mode-hook #'typo-mode)


# Applications


## Org mode

I’ve put org mode under Applications, as opposed to Writing, because it’s  more generally-applicable than that.


### Basics

    (straight-use-package 'org)
    
    (with-eval-after-load 'org
      (require 'org-tempo)
      (require 'ox-md)
      (define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)
      (define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading))
    
    (cuss org-hide-emphasis-markers t)
    (cuss org-fontify-done-headline t)
    (cuss org-fontify-whole-heading-line t)
    (cuss org-fontify-quote-and-verse-blocks t)
    (cuss org-pretty-entities t)
    (cuss org-num-mode +1)
    (cuss org-src-tab-acts-natively t)
    (cuss org-src-fontify-natively t)
    (cuss org-src-window-setup 'current-window)
    (cuss org-confirm-babel-evaluate nil)
    (cuss org-directory "~/Org")


### Org Agenda

    (cuss org-agenda-files (no-littering-expand-etc-file-name "agenda-files"))
    
    (if (and (stringp org-agenda-files)
    	 (not (file-exists-p org-agenda-files)))
        (with-temp-buffer (write-file org-agenda-files)))
    
    (define-key acdw/map (kbd "C-a") #'org-agenda)


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


### Insert blank lines around headers

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


# Appendices


## Emacs' files


### init.el

I realized I didn’t need `early-init.el`, since it really only set `load-prefer-newer`.  So I’ve set that here, and wrapped the actual loading of config in a `let*` form that speeds up init, and loads the newer of either `config.org` or `config.el`.

    ;; init.el -*- lexical-binding: t -*-
    
    (setq load-prefer-newer t)
    
    (let* (;; Speed up init
           (gc-cons-threshold most-positive-fixnum)
           (file-name-handler-alist nil)
           ;; Config file names
           (conf (expand-file-name "config"
    			       user-emacs-directory))
           (conf-el (concat conf ".el"))
           (conf-org (concat conf ".org")))
      (unless (and (file-newer-than-file-p conf-el conf-org)
    	       (load conf 'no-error))
        (require 'org)
        (org-babel-load-file conf-org)))


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
    	    (message "Exporting README.md...")
    	    (require 'ox-md)
    	    (with-demoted-errors "Problem exporting README.md: %S"
    	      (org-md-export-to-markdown)))
    	  ;; tangle config.org
    	  (when (file-newer-than-file-p config (expand-file-name
    						"config.el"
    						user-emacs-directory))
    	    (message "Tangling config.org...")
    	    (require 'org)
    	    (let ((inits (org-babel-tangle)))
    	      ;; byte-compile resulting files
    	      (message "Byte-compiling...")
    	      (dolist (f inits)
    		(when (string-match "\\.el\\'" f)
    		  (byte-compile-file f (not disable-load)))))))))))


## Ancillary scripts


### emacsdc

Here's a wrapper script that'll start `emacs –daemon` if there isn't
one, and then launch `emacsclient` with the arguments.  I'd recommend
installing with either `ln -s bin/emacsdc $HOME/.local/bin/`, or
adding `$HOME/.local/bin` to your `$PATH`.

    if ! emacsclient -nc "$@" 2>/dev/null; then
        emacs --daemon
        emacsclient -nc "$@"
    fi


### Emacs.cmd

Here’s a wrapper script that’ll run Emacs on Windows, with a custom `$HOME`.  I have mine setup like this: Emacs is downloaded from [the GNU mirror](https://mirrors.tripadvisor.com/gnu/emacs/windows/emacs-27/emacs-27.1-x86_64.zip) and unzipped to `~/Downloads/emacs/`.  `Emacs.cmd` sets `$HOME` to `~/Downloads/emacshome/`, which is where `.emacs.d` is, and whatever else I might want to throw in there.

    set HOME=%~dp0..\..\emacshome
    "%~dp0runemacs.exe" %*


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

