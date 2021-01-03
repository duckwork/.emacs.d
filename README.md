# Basics


## Disclaimer

    ;; config.el -*- lexical-binding: t -*-
    ;; This file is automatically tangled from config.org.
    ;; Hand edits will be overwritten!


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
      (message "%s" "Straight.el didn't bootstrap correctly.  Cloning directly...")
      (call-process "git" nil
		(get-buffer-create "*bootstrap-straight-messages*") nil
		"clone"
		"https://github.com/raxod502/straight.el"
		(expand-file-name "straight/repos/straight.el"
				  user-emacs-directory))
      (acdw/bootstrap-straight))


## Ease-of-configuring functions


### Emulate use-package’s `:custom`

    (defmacro cuss (var val &optional docstring)
      "Basically, `:custom' from `use-package', but without `use-package'."
      (declare (doc-string 3)
	   (indent 2))
      `(funcall (or (get ',var 'custom-set) #'set-default)
	    ',var ,val))


### Emulate use-package’s `:custom-face`, but better

    (defvar acdw--custom-faces ()
      "List of custom faces to run through acdw/set-custom-faces.")
    
    (defun acdw/set-custom-faces ()
      "Run `customize-set-faces' on `acdw--custom-faces'."
      (message "%s" "Customizing faces...")
      (apply #'custom-set-faces acdw--custom-faces))
    
    (defun cussface (spec)
      "Add SPEC to `acdw--custom-faces', and add a hook to run
    `acdw/set-custom-faces' after init."
      (add-to-list 'acdw--custom-faces spec)
      (add-hook 'after-init-hook #'acdw/set-custom-faces))


### Determine whether any Emacs frame is focused or not

This comes in handy when I want to garbage collect, say, or save recent files.

    (defun acdw/when-unfocused (func &rest args)
      "Run FUNC with ARGS only if all frames are out of focus."
      (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
	  (apply func args)))


## Clean `.emacs.d`

    (straight-use-package 'no-littering)
    (require 'no-littering)


### Don’t clutter `init.el` with customizations

    (with-eval-after-load 'no-littering 
      (cuss custom-file (no-littering-expand-etc-file-name "custom.el")))


## Look and feel


### Cursor

    (cuss cursor-type 'bar
      "Show a vertical bar for the cursor.")
    
    (cuss cursor-in-non-selected-windows 'hbar
      "Show an empty box in inactive windows.")
    
    ;; Don't blink the cursor
    (blink-cursor-mode -1)


### Tool Bars

1.  Tool bars and menu bars

	(menu-bar-mode -1)
	(tool-bar-mode -1)

2.  Scroll bars

	(scroll-bar-mode -1)
	(horizontal-scroll-bar-mode -1)


### Dialogs

    (cuss use-dialog-box nil
      "Don't use dialog boxes to ask questions.")

1.  Yes or no questions

	(fset 'yes-or-no-p #'y-or-n-p)

2.  The Bell

    from [EmacsWiki](https://www.emacswiki.org/emacs/AlarmBell#h5o-3).
    
	(setq visible-bell nil
	      ring-bell-function 'flash-mode-line)
        
	(defun flash-mode-line ()
	  (invert-face 'mode-line)
	  (run-with-timer 0.1 nil #'invert-face 'mode-line))


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

3.  Kill the current buffer

	(defun acdw/kill-a-buffer (&optional prefix)
	  "Kill a buffer based on the following rules:
        
	C-x k     ⇒ Kill current buffer & window
	C-u C-x k ⇒ Kill OTHER window and its buffer
	C-u C-u C-x C-k ⇒ Kill all other buffers and windows
        
	Prompt only if there are unsaved changes."
	  (interactive "P")
	  (pcase (or (car prefix) 0)
	    ;; C-x k     ⇒ Kill current buffer & window
	    (0  (kill-current-buffer)
		(unless (one-window-p) (delete-window)))
	    ;; C-u C-x k ⇒ Kill OTHER window and its buffer
	    (4  (other-window 1)
		(kill-current-buffer)
		(unless (one-window-p) (delete-window)))
	    ;; C-u C-u C-x C-k ⇒ Kill all other buffers and windows
	    (16   (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
		  (delete-other-windows))))
        
	(define-key ctl-x-map "k" #'acdw/kill-a-buffer)
    
    1.  Remap `C-x M-k` to bring up the buffer-killing menu
    
	    (define-key ctl-x-map (kbd "M-k") #'kill-buffer)

4.  Immortal `*scratch*` buffer

	(defun immortal-scratch ()
	  (if (eq (current-buffer) (get-buffer "*scratch*"))
	      (progn (bury-buffer)
		     nil)
	    t))
        
	(add-hook 'kill-buffer-query-functions 'immortal-scratch)


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
        
	;; sunrise
	(run-at-time (nth 1 (split-string (sunrise-sunset)))
		     (* 60 60 24)
		     (lambda ()
		       (modus-themes-load-operandi)))
        
	;; sunset
	(run-at-time (nth 4 (split-string (sunrise-sunset)))
		     (* 60 60 24)
		     (lambda ()
		       (modus-themes-load-vivendi)))


### Fonts

1.  Define fonts

	(defun set-face-from-alternatives (face fonts)
	  (catch :return
	    (dolist (font fonts)
	      (when (find-font (font-spec :family (car font)))
		(apply #'set-face-attribute `(,face
					      nil
					      :family (car font)
					      ,@(cdr font)))
		(throw :return font)))))
        
	(defun acdw/setup-fonts ()
	  "Setup fonts.  This has to happen after the frame is setup for
	the first time, so it should be added to `window-setup-hook'.  It
	removes itself from that hook."
	  (interactive)
	  (when (display-graphic-p)
	    (set-face-from-alternatives 'default
					'(("Input Mono"
					   :height 105)
					  ("Go Mono"
					   :height 100)
					  ("Consolas"
					   :height 100)))
        
	    (set-face-from-alternatives 'fixed-pitch
					'(("Input Mono")
					  ("Go Mono")
					  ("Consolas")))
        
	    (set-face-from-alternatives 'variable-pitch
					'(("Input Serif")
					  ("Georgia")))
        
	    (remove-function after-focus-change-function #'acdw/setup-fonts)))
        
	(add-function :before after-focus-change-function #'acdw/setup-fonts)

2.  Custom faces

	(cussface '(font-lock-comment-face
		    ((t (:inherit (custom-comment italic variable-pitch))))))

3.  Line spacing

	(cuss line-spacing 0.1
	  "Add 10% extra space below each line.")

4.  Underlines

	(cuss x-underline-at-descent-line t
	  "Draw the underline at the same place as the descent line.")

5.  Unicode Fonts

	(straight-use-package 'unicode-fonts)
	(require 'unicode-fonts)
	(unicode-fonts-setup)


## Interactivity


### Completing read

1.  Shadow file names in `completing-read`.

	(cuss file-name-shadow-properties '(invisible t))
        
	(file-name-shadow-mode +1)

2.  Ignore case in `completing-read`

	(cuss completion-ignore-case t)
	(cuss read-buffer-completion-ignore-case t)
	(cuss read-file-name-completion-ignore-case t)

3.  Minibuffer recursivity

	(cuss enable-recursive-minibuffers t)
	(minibuffer-depth-indicate-mode +1)

4.  Selectrum

	(straight-use-package 'selectrum)
	(require 'selectrum)
	(selectrum-mode +1)

5.  Prescient

	(straight-use-package 'prescient)
	(require 'prescient)
	(prescient-persist-mode +1)
        
	(straight-use-package 'selectrum-prescient)
	(require 'selectrum-prescient)
	(selectrum-prescient-mode +1)

6.  Consult

	(straight-use-package '(consult
				:host github
				:repo "minad/consult"))
	(require 'consult)
        
	(straight-use-package '(consult-selectrum
				:host github
				:repo "minad/consult"))
	(require 'consult-selectrum)
        
	(with-eval-after-load 'consult
	  (define-key ctl-x-map "b" #'consult-buffer)
	  (define-key ctl-x-map (kbd "C-r") #'consult-buffer)
	  (define-key ctl-x-map "4b" #'consult-buffer-other-window)
	  (define-key ctl-x-map "5b" #'consult-buffer-other-frame)
        
	  (define-key goto-map "o" #'consult-outline)
	  (define-key goto-map "g" #'consult-line)
	  (define-key goto-map (kbd "M-g") #'consult-line)
	  (define-key goto-map "l" #'consult-line)
	  (define-key goto-map "m" #'consult-mark)
	  (define-key goto-map "i" #'consult-imenu)
	  (define-key goto-map "e" #'consult-error)
        
	  (global-set-key (kbd "M-y") #'consult-yank-pop)
        
	  (define-key help-map "a" #'consult-apropos)
        
	  (fset 'multi-occur #'consult-multi-occur))

7.  Marginalia

	(straight-use-package '(marginalia
				:host github
				:repo "minad/marginalia"
				:branch "main"))
        
	(cuss marginalia-annotators
	    '(marginalia-annotators-heavy
	      marginalia-annotators-light))
        
	(marginalia-mode +1)


### Completion

    (global-set-key (kbd "M-/") #'hippie-expand)


### Garbage collection

    (straight-use-package 'gcmh)
    (gcmh-mode +1)
    
    (defun dotfiles--gc-on-last-frame-out-of-focus ()
      "GC if all frames are inactive."
      (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
	  (garbage-collect)))
    
    (add-function :after after-focus-change-function
	      #'dotfiles--gc-on-last-frame-out-of-focus)


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


### Show keybindings

    (straight-use-package 'which-key)
    (which-key-mode +1)


## Mouse


### Preserve screen position when scrolling with the mouse wheel

from [u/TheFrenchPoulp](https://www.reddit.com/r/emacs/comments/km9by4/weekly_tipstricketc_thread/ghg2c9d/).

    (advice-add 'mwheel-scroll :around #'me/mwheel-scroll)
    
    (defun me/mwheel-scroll (original &rest arguments)
      "Like `mwheel-scroll' but preserve screen position.
    See `scroll-preserve-screen-position'."
      (let ((scroll-preserve-screen-position :always))
	(apply original arguments)))


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
    
    ;; save recentf list when focusing away
    (defun acdw/maybe-save-recentf ()
      "Save `recentf-file' when out of focus, but only if we haven't
    in five minutes."
      (defvar recentf-last-save (time-convert nil 'integer)
	"How long it's been since we last saved the recentf list.")
    
      (when (> (time-convert (time-since recentf-last-save) 'integer)
	   (* 60 5))
	(setq recentf-last-save (time-convert nil 'integer))
	(acdw/when-unfocused #'recentf-save-list)))
    
    (add-function :after after-focus-change-function
	      #'acdw/maybe-save-recentf)


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

1.  Replace with Anzu

	  (straight-use-package 'anzu)
	  (require 'anzu)
        
	  ;; show search count in the modeline
	  (global-anzu-mode +1)
        
	  (cuss anzu-replace-to-string-separator " → "
	    "What to separate the search from the replacement.")
        
	(global-set-key [remap query-replace] #'anzu-query-replace)
	(global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)
        
	(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
	(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)


### Overwrite mode

    (defun acdw/overwrite-mode-change-cursor ()
      (setq cursor-type (if overwrite-mode t 'bar)))
    
    (add-hook 'overwrite-mode-hook #'acdw/overwrite-mode-change-cursor)
    
    (rm/whitelist-add "Ovwrt")


### The Mark

    (cuss set-mark-repeat-command-pop t
      "Repeat `set-mark-command' with a prefix argument, without
      repeatedly entering the prefix argument.")


### Whitespace

    (cuss whitespace-style
	'(empty ;; remove blank lines at the beginning and end of buffers
	  indentation ;; clean up indentation
	  space-before-tab ;; fix mixed spaces and tabs
	  space-after-tab))
    
    (add-hook 'before-save-hook #'whitespace-cleanup)


# Programming


## Prettify symbols


### A bit from Rasmus

from [Rasmus Roulund](https://pank.eu/blog/pretty-babel-src-blocks.html#coderef-symbol), via [Musa Al-hassy](https://alhassy.github.io/emacs.d/index.html#Making-Block-Delimiters-Less-Intrusive).

    (defvar-local rasmus/org-at-src-begin -1
      "Variable that holds whether last position was a ")
    
    (defvar rasmus/ob-header-symbol ?☰
      "Symbol used for babel headers")
    
    (defun rasmus/org-prettify-src--update ()
      (let ((case-fold-search t)
	(re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
	found)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	(goto-char (match-end 0))
	(let ((args (org-trim
		     (buffer-substring-no-properties (point)
						     (line-end-position)))))
	  (when (org-string-nw-p args)
	    (let ((new-cell (cons args rasmus/ob-header-symbol)))
	      (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
	      (cl-pushnew new-cell found :test #'equal)))))
	  (setq prettify-symbols-alist
	    (cl-set-difference prettify-symbols-alist
			       (cl-set-difference
				(cl-remove-if-not
				 (lambda (elm)
				   (eq (cdr elm) rasmus/ob-header-symbol))
				 prettify-symbols-alist)
				found :test #'equal)))
	  ;; Clean up old font-lock-keywords.
	  (font-lock-remove-keywords nil prettify-symbols--keywords)
	  (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
	  (font-lock-add-keywords nil prettify-symbols--keywords)
	  (while (re-search-forward re nil t)
	(font-lock-flush (line-beginning-position) (line-end-position))))))
    
    (defun rasmus/org-prettify-src ()
      "Hide src options via `prettify-symbols-mode'.
    
      `prettify-symbols-mode' is used because it has uncollpasing. It's
      may not be efficient."
      (let* ((case-fold-search t)
	 (at-src-block (save-excursion
			 (beginning-of-line)
			 (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
	;; Test if we moved out of a block.
	(when (or (and rasmus/org-at-src-begin
		   (not at-src-block))
	      ;; File was just opened.
	      (eq rasmus/org-at-src-begin -1))
	  (rasmus/org-prettify-src--update))
	;; Remove composition if at line; doesn't work properly.
	;; (when at-src-block
	;;   (with-silent-modifications
	;;     (remove-text-properties (match-end 0)
	;;                             (1+ (line-end-position))
	;;                             '(composition))))
	(setq rasmus/org-at-src-begin at-src-block)))
    
    (defun rasmus/org-prettify-symbols ()
      (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
	(cl-reduce 'append
		   (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
			   `(("#+begin_src" . ?✎) ;; ➤ 🖝 ➟ ➤ ✎
			     ("#+end_src"   . ?⏹) ;; □
			     ("#+header:" . ,rasmus/ob-header-symbol)
			     ("#+begin_quote" . ?»)
			     ("#+end_quote" . ?«)))))
      (turn-on-prettify-symbols-mode)
      (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))


### Regular prettify-symbols-mode

    (cuss prettify-symbols-unprettify-at-point 'right-edge
      "Unprettify a symbol when inside it or next to it.")
    
    (defun acdw/org-mode-prettify ()
      "Prettify `org-mode'."
      (append '(("[ ]" . ?□) ("[X]" . ?☑) ("[-]" . ?◐)
	    ("#begin_src" . ?🖬) ("#BEGIN_SRC" . ?🖬)
	    ("#end_src" . ?■) ("#END_SRC" . ?■))
	  prettify-symbols-alist))
    
    (add-hook 'org-mode-hook #'acdw/org-mode-prettify)
    
    (global-prettify-symbols-mode +1)


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

    (dolist (margin '(right-margin left-margin))
      (dolist (button '(mouse-1 mouse-2 mouse-3))
	(global-set-key (vector margin button)
		    (global-key-binding (vector button)))))
    
    (mouse-wheel-mode +1)
    
    (when (bound-and-true-p mouse-wheel-mode)
      (dolist (margin '(right-margin left-margin))
	(dolist (event '(mouse-wheel-down-event
		     mouse-wheel-up-event
		     wheel-down
		     wheel-up
		     mouse-4
		     mouse-5))
	  (global-set-key (vector margin event) #'mwheel-scroll))))


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


### Variable pitch in text-modes

    (add-hook 'text-mode-hook #'variable-pitch-mode)


### Typo mode

    (straight-use-package 'typo)
    
    (add-hook 'text-mode-hook #'typo-mode)
    
    ;; Disable `typo-mode' when inside an Org source block
    (with-eval-after-load 'typo
      (add-to-list 'typo-disable-electricity-functions
	       #'org-in-src-block-p))


## Word count

    (straight-use-package 'wc-mode)
    
    (add-hook 'text-mode-hook #'wc-mode)
    
    (rm/whitelist-add "WC")


# Applications


## Dired


### Expand subtrees

    (straight-use-package 'dired-subtree)
    
    (with-eval-after-load 'dired
      (define-key dired-mode-map "i" #'dired-subtree-toggle))


### Collapse singleton directories

    (straight-use-package 'dired-collapse)
    
    (add-hook 'dired-mode-hook #'dired-collapse-mode)


## Org mode

I’ve put org mode under Applications, as opposed to Writing, because it’s  more generally-applicable than that.


### Basics

    (straight-use-package '(org
			:repo "https://code.orgmode.org/bzg/org-mode.git"))
    
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
    (cuss org-src-tab-acts-natively t)
    (cuss org-src-fontify-natively t)
    (cuss org-src-window-setup 'current-window)
    (cuss org-confirm-babel-evaluate nil)
    (cuss org-directory "~/Org")
    (cuss org-ellipsis "…")
    (cuss org-catch-invisible-edits 'show)

1.  Tags

	(cuss org-tags-column 0
	  "Show tags directly after the headline.
	This is the best-looking option with variable-pitch fonts.")
        
	(cussface
	 '(org-tag
	   ((t
	     (:height 0.8 :weight normal :slant italic :foreground "grey40" :inherit
		      (variable-pitch))))))


### General

1.  [Org Return: DWIM](https://github.com/alphapapa/unpackaged.el#org-return-dwim)

	(defun unpackaged/org-element-descendant-of (type element)
	  "Return non-nil if ELEMENT is a descendant of TYPE.
	TYPE should be an element type, like `item' or `paragraph'.
	ELEMENT should be a list like that returned by `org-element-context'."
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
        
	     ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
	     ;; followed.
        
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
		       (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n"))) nil))
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
        
	(with-eval-after-load 'org
	  (define-key org-mode-map (kbd "RET") #'unpackaged/org-return-dwim))

2.  Insert blank lines around headers

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


### Org Agenda

    (cuss org-agenda-files
	(let ((list))
	  (dolist (file '(;; add more files to this list
		      "home.org"
		      "work.org")
		    list)
	(push (expand-file-name file org-directory) list))))
    
    (define-key acdw/map (kbd "C-a") #'org-agenda)
    
    (cuss org-todo-keywords
	'((sequence "RECUR(r)" "TODO(t)" "|" "DONE(d)")
	  (sequence "|" "CANCELLED(c)")))


### TODO Capture


### Include Org links in source code

    (straight-use-package '(org-link-minor-mode
			:host github
			:repo "seanohalpin/org-link-minor-mode"))
    
    ;; enable in elisp buffers
    (add-hook 'emacs-lisp-mode-hook #'org-link-minor-mode)


## Git

    (straight-use-package 'magit)
    
    (define-key acdw/map "g" #'magit-status)


## Beancount mode

    (straight-use-package '(beancount-mode
			:host github
			:repo "beancount/beancount-mode"))
    (require 'beancount)
    
    (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
    
    (defun acdw/disable-aggressive-indent ()
      "Turn `aggressive-indent-mode' off for a buffer."
      (aggressive-indent-mode -1))
    
    (add-hook 'beancount-mode-hook #'outline-minor-mode)
    (add-hook 'beancount-mode-hook #'acdw/disable-aggressive-indent)
    
    (define-key beancount-mode-map (kbd "M-n") #'outline-next-visible-heading)
    (define-key beancount-mode-map (kbd "M-p") #'outline-previous-visible-heading)


# Appendices


## Emacs' files


### init.el

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


### early-init.el

    ;; early-init.el -*- no-byte-compile: t; -*-
    
    ;; I use `straight.el' instead of `package.el'.
    (setq package-enable-at-startup nil)
    
    ;; Don't resize the frame when loading fonts
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
	    (message "%s" "Exporting README.md...")
	    (require 'ox-md)
	    (with-demoted-errors "Problem exporting README.md: %S"
	      (org-md-export-to-markdown)))
	  ;; tangle config.org
	  (when (file-newer-than-file-p config (expand-file-name
						"config.el"
						user-emacs-directory))
	    (message "%s" "Tangling config.org...")
	    (require 'org)
	    (let ((inits (org-babel-tangle)))
	      ;; byte-compile resulting files
	      (message "%s" "Byte-compiling...")
	      (dolist (f inits)
		(when (string-match "\\.el\\'" f)
		  (byte-compile-file f (not disable-load)))))))))))


## Ancillary scripts


### emacsdc

:header-args: :tangle bin/emacsdc :tangle-mode (identity #o755)

Here's a wrapper script that'll start `emacs --daemon` if there isn't
one, and then launch `emacsclient` with the arguments.  I'd recommend
installing with either `ln -s bin/emacsdc $HOME/.local/bin/`, or
adding `$HOME/.local/bin` to your `$PATH`.

    if ! emacsclient -nc "$@" 2>/dev/null; then
	emacs --daemon
	emacsclient -nc "$@"
    fi


### Emacs.cmd

Here's a wrapper script that'll run Emacs on Windows, with a custom `$HOME`.  I have
mine setup like this: Emacs is downloaded from [the GNU mirror](https://mirrors.tripadvisor.com/gnu/emacs/windows/emacs-27/emacs-27.1-x86_64.zip) and unzipped to
`~/Downloads/emacs/`.  For some reason, Emacs by default sets `$HOME` to `%APPDATA%`,
which doesn’t make a lot of sense to me.  I change it to `%USERPROFILE%`, and then run
Emacs with the supplied arguments.

    set HOME=%USERPROFILE%
    set EMACS="%USERPROFILE%\Downloads\emacs\bin\runemacs.exe"
    
    REM Run "Quick Mode"
    REM "%EMACS%" -Q %*
    
    REM Regular
    "%EMACS%" %*


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
