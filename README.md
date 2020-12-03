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

This file replaces itself with the actual configuration when it's first run.  For easy installation, *this* is the `init.el` file in git &#x2013; and you probably want to keep it that way.  To keep git from trying to update `init.el` when it's re-tangled, type this in the repo:

	git update-index --assume-unchanged init.el

If, for some reason, you want to change this original file to be re-tracked, run this command:

	git update-index --no-assume-unchanged init.el

Otherwise, here's the actual, original `init.el` that tangles this Org file and gets us going.

	(require 'org)
	(find-file (concat user-emacs-directory "config.org"))
	(org-babel-tangle)
	(load-file (concat user-emacs-directory "early-init.el"))
	(load-file (concat user-emacs-directory "init.el"))
	(byte-compile-file (concat user-emacs-directory "init.el"))


### TODO What I should do instead

Honestly, I should just change this "Original init.el" thing to a Makefile I can tangle in `config.org`, and track &#x2013; since it won't be overwritten or need any special `git` invocations to stop tracking it, I can edit it as I think about what would work best.  I could also maybe give it more of a "cross-platform" vibe by installing, say, `straight.el` in the Makefile on Windows.  One day &#x2026;


## Tangling

After our first tangle, each time we edit `config.org` we want to go ahead and re-tangle our config.  To that end, I've written `acdw/tangle-init`, which automatically tangles `config.org`.

	(defun acdw/tangle-init ()
	  "If the current buffer is `config.org', tangle it, then compile
	and load the resulting files."
	  (when (equal (buffer-file-name)
    	       (expand-file-name
    		(concat user-emacs-directory "config.org")))
		;; Tangle and load init.el and early-init.el
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

Since I want to tangle every time I save `config.org`, I've added `acdw/tangle-init` to a hook.

	(add-hook 'after-save-hook #'acdw/tangle-init)

Finally, I want an easier way to load the generated init files than the old `M-x load-file RET ~/.config/emacs/init.el RET`.  So I've written `acdw/load-init` &#x2013; which also gets called at the end of the async part of `acdw/tangle-init`.

	(defun acdw/load-init ()
	  (interactive)
	  (load-file (expand-file-name
    	      (concat user-emacs-directory "early-init.el")))
	  (load-file (expand-file-name
    	      (concat user-emacs-directory "init.el"))))


## Miscellaneous bootstrappy stuff


### Add directories to `load-path`

I also put lispy stuff in the `lisp/` subdirectory of my Emacs config, and under my SyncThing directory (for easy syncing ;P).

	(dolist (dir `(,(concat user-emacs-directory
    			(convert-standard-filename "lisp/"))
    	       ,(expand-file-name "~/Sync/elisp/")))
	  (add-to-list 'load-path dir))


### TODO Require my secrets

While this is like, the *dumbest* way to do this, it's what I'm doing right now.  I'm going to slap a TODO on here because I really should make it better &#x2013; like, `auth-sources` hooked into KeePassXC somehow&#x2026; ?  Maybe follow [Bill Dietrich's setup](https://www.billdietrich.me/Authentication.html?expandall=1#KeePassXCandSecretService).

	(require 'acdw-secrets)


# Early initiation

Starting with version 27.1, Emacs loads `early-init.el` *before* `init.el`, setting up early stuff like package management, etc.  Since I use an alternative package manager, I have to bootstrap it here.

Of course, I also want to set some really early-on settings here too, like `load-prefer-newer` &#x2013; why not?

	;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-
	  (setq load-prefer-newer t)


## Increase the garbage collector

Let's try to speed startup times by increasing the garbage collector's threshold while running init.  Note the hook afterwards that restores it to a reasonable default.

	(setq gc-cons-threshold (* 100 100 1000))

	(add-hook 'after-init-hook
    	  (lambda ()
    	    (setq gc-cons-threshold (* 100 100 100))
    	    (message "gc-cons-threshold restored to %S"
    		     gc-cons-threshold)))


## Add more paths to the `exec-path`

When using Windows (at work), I need to use the PortableGit installation I've downloaded, since I don't have Admin privileges.

	(when (eq system-type 'windows-nt)
	  (dolist (path '("c:/Users/aduckworth/Downloads/emacs/bin"
    		  "C:/Users/aduckworth/Downloads/PortableGit/bin"
    		  "C:/Users/aduckworth/Downloads/PortableGit/usr/bin"))
		(add-to-list 'exec-path path)))

Elsewhere, I want to add a few more paths to the `exec-path` as well, since I store scripts in a couple of places at ~.

	(dolist (path `(,(expand-file-name "bin"
    				   user-emacs-directory)
    		,(expand-file-name "~/bin")
    		,(expand-file-name "~/.local/bin")
    		,(expand-file-name "~/Scripts")))
	  (add-to-list 'exec-path path))


## Bootstrap [straight.el](https://github.com/raxod502/straight.el)

So far, this is the best package manager I've used.  It allows for *truly* declarative package management (if I don't specify a package here, it doesn't get loaded), easy installation from pretty much any source (as long as it's got a git repo), *and* it hooks into `use-package`!

The one annoying thing is that this bootstrap code doesn't work on Windows for some reason.  I'm too lazy to really try and figure out why, so when I need to bootstrap on Windows (pretty rare, TBH), I just [download the master-branch zip file](https://github.com/raxod502/straight.el/archive/master.zip) and extract it to `~/.emacs.d/straight/repos/`.

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


## Use [use-package](https://jwiegley.github.io/use-package/)

Like I said, `straight.el` hooks into `use-package` easily.  These two lines get the latter to use the former by default.

	(setq straight-use-package-by-default t)
	(straight-use-package 'use-package)


## Keep `~/.emacs.d` tidy with [no-littering](https://github.com/emacscollective/no-littering)

I'll be honest &#x2013; I don't really notice this package.  But I think that's the point.  It keeps Emacs (and packages) from throwing files all over the place, so I have a clean `ls -l`.  Since I want to run this code as early as possible, I use the `straight-use-package` form instead of `use-package`.

	(straight-use-package 'no-littering)
	(require 'no-littering)


## Additional `use-package` keywords


### [:custom-update](https://github.com/a13/use-package-custom-update)

The `:custom-update` keyword lets me do this:

	(use-package package
	  :custom-update
	  (package-list '(1 2 3)))

instead of this:

	(use-package package
	  :config
	  (add-to-list 'package-list '(1 2 3)))

It's not &#x2026; perfect, but it's kind of nice.

	(use-package use-package-custom-update
	  :straight (use-package-custom-update
    	     :host github
    	     :repo "a13/use-package-custom-update"))


## Setup [async](https://github.com/jwiegley/emacs-async)

I thought this was included in Emacs at first, but it's not &#x2013; so we need to install and require it.

	(straight-use-package 'async)
	(require 'async)


# Macros


## Customizing variables

I like `use-package` a lot, but I don't like using those shims you see in a lot of other Emacs configs where they use `(use-package emacs)` forms and stuff like that &#x2013; it just feels dirty.  Plus, `straight` gets confused about those packages sometimes.  So, since I'm actually *configuring* Emacs in this Org file, which is nicely organized anyway, I can just set settings the old-school way.

Except.  Using `setq` is actually *not* recommended any more, because `customize-set-variable` is more expressive and can include side-effects.  However, not all settings are customizable, *and* `customize-set-variable` is like, way longer to type.  So I've decided to write a little macro (my first!) to copy `use-package`'s `:custom` keyword, except &#x2026; *outside* `use-package`.  I've called it `cuss`, because I have a terrible sense of humor.

	(defmacro cuss (var val)
	  "Basically `use-package''s `:custom', but without using either."
	  `(progn
		 (funcall (or (get ',var 'custom-set) #'set-default)
    	      ',var ,val)))


# Theme: [Modus](https://protesilaos.com/modus-themes/)

Protesilaos Stavrou's *excellent* theme pair.  At some point I'll probably write my own that's really minimal and does some funky stuff with faces, but until then, these really are the best I've used.

The big `dolist` form is from [his documentation](https://protesilaos.com/modus-themes/#h:a897b302-8e10-4a26-beab-3caaee1e1193); it basically allows me to configure both themes before loading them.  I've tweaked his code a little to use `use-package`.

	(defmacro modus-themes-format-sexp (sexp &rest objects)
	  `(eval (read (format ,(format "%S" sexp) ,@objects))))

	(dolist (theme '("operandi" "vivendi"))
	  (modus-themes-format-sexp
	   (use-package modus-%1$s-theme
		 :custom
		 (modus-%1$s-theme-slanted-constructs t)
		 (modus-%1$s-theme-bold-constructs t)
		 (modus-%1$s-theme-fringes nil)
		 (modus-%1$s-theme-mode-line '3d)
		 (modus-%1$s-theme-syntax 'yellow-comments)
		 (modus-%1$s-theme-intense-hl-line nil)
		 (modus-%1$s-theme-intense-paren-match t)
		 (modus-%1$s-theme-links nil)
		 (modus-%1$s-theme-no-mixed-fonts nil)
		 (modus-%1$s-theme-prompts nil)
		 (modus-%1$s-theme-completions nil)
		 (modus-%1$s-theme-diffs nil)
		 (modus-%1$s-theme-org-blocks 'grayscale)
		 (modus-%1$s-theme-headings
		  '((1 . section)
    	(2 . line)
    	(t . rainbow-line)))
		 (modus-%1$s-theme-variable-pitch-headings t)
		 (modus-%1$s-theme-scale-headings t)
		 (modus-%1$s-theme-scale-1 1.1)
		 (modus-%1$s-theme-scale-2 1.15)
		 (modus-%1$s-theme-scale-3 1.21)
		 (modus-%1$s-theme-scale-4 1.27)
		 (modus-%1$s-theme-scale-5 1.33)
		 :custom-face
		 (font-lock-comment-face
		  ((t (:inherit (custom-comment italic variable-pitch))))))
	   theme))


## Theme changer

I also want to switch themes between night and day.

	(use-package theme-changer
	  :custom
	  (calendar-latitude 30.39)
	  (calendar-longitude -91.83)
	  :config
	  (change-theme 'modus-operandi 'modus-vivendi))


# Simplify GUI


<a id="org6553b8c"></a>

## Frame defaults

I want no toolbar, menubar, or scrollbars (ideally I'd have a vertical scrollbar if necessary, but apparently that's too much to ask the Emacs devs); and fringes and window dividers 2 pixels wide.

	(cuss default-frame-alist
		  '((tool-bar-lines . 0)
    	(menu-bar-lines . 0)
    	(vertical-scroll-bars . nil)
    	(horizontal-scroll-bars . nil)
    	(right-divider-width . 2)
    	(bottom-divider-width . 2)
    	(left-fringe-width . 2)
    	(right-fringe-width . 2)))


## Minibuffer window/frame defaults

Of course, on the minibuffer, I want to make sure there's no scrollbar &#x2013; even if I change my mind on `vertical-scroll-bars`, above.

	(cuss minibuffer-frame-alist
		  '((width . 80)
    	(height . 2)
    	(vertical-scrollbars . nil)))

	(set-window-scroll-bars (minibuffer-window) nil nil)


## Remove unneeded GUI elements

The [Frame Defaults](#org6553b8c) section sets up the frame to be free of visual clutter, but *this* section allows us to toggle that clutter's visibility easily, with one call to each of these functions.

	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(horizontal-scroll-bar-mode -1)


## Tabs

I'm kind of getting into Emacs tabs &#x2013; but I like not showing the `tab-bar` when there's only one.

	(cuss tab-bar-show 1)

	(cuss tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)


## Word wrap and operate visually

`global-visual-line-mode` is one of those which, in my opinion, should be a default.  There's only one place I don't want to wrap words, and that's in `dired`, which I can set individually in its config.

	(global-visual-line-mode 1)


## Modeline


### [smart-mode-line](https://github.com/Malabarba/smart-mode-line)

	(use-package smart-mode-line
	  :custom
	  (sml/no-confirm-load-theme t)
	  :config
	  (sml/setup))


### [rich-minority](https://github.com/Malabarba/rich-minority)

`smart-mode-line` comes with `rich-minority` for taking care of minor modes in the modeline, so I'm not going to *also* use `diminish` or anything.  However, `rich-minority` has kind of a hinky way of adding modes to the whitelist, so I had to write my own function to do so.

This confuration means that, by default, no minor modes are shown; if you want  a minor mode to be shown (like `word-count-mode` for me), call `(rm/whitelist-add "REGEXP")`.

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


## Minibuffer


### Keep cursor from going into the prompt

from [Ergo Emacs](http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html).

	(cuss minibuffer-prompt-properties
		  '(read-only t cursor-intangible t face minibuffer-prompt))


## Show `^L` as a line

I like using the form-feed character to separate pages, it turns out.  'Tis nice.  This package turns that character into a nice long line.

	(use-package form-feed
	  :hook
	  ((text-mode prog-mode) . form-feed-mode))


## Cursor

I want my cursor to be a bar in focused windows, but a hollow box in non-focused windows.

	(cuss cursor-type 'bar)
	(cuss cursor-in-non-selected-windows 'hollow)


# Typesetting


## Fonts

This is the best way I've come up with to specify a number of different fonts that apply depending on what's applied.  To be honest, I didn't really come up with the `font-candidate` function, though &#x2013; I got it from the ["Testing if fonts are available?"](https://www.emacswiki.org/emacs/SetFonts#toc11) section of the SetFonts page on EmacsWiki.

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
    		       "Libertinus Serif-13"
    		       "Linux Libertine O-12"
    		       "Georgia-11"))

	  (remove-hook 'focus-in-hook #'acdw/setup-fonts))

	(add-hook 'focus-in-hook #'acdw/setup-fonts)


## [unicode-fonts](https://github.com/rolandwalker/unicode-fonts)

This does something similar to the above code, but for the entirety of the Unicode field (I think).

	(use-package unicode-fonts
	  :config
	  (unicode-fonts-setup))


## Variable pitch faces

One reason I like the Modus themes so much is that they have *excellent* support for variable-pitch faces, and mixing them with fixed-pitch faces in, say, Org Mode.  That means I can enable `variable-pitch-mode` in all my `text-mode`-derived buffers.

	(add-hook 'text-mode-hook #'variable-pitch-mode)


## Padding

This has been taken from ["Ricing Org Mode"](https://lepisma.xyz/2017/10/28/ricing-org-mode/) &#x2013; of course, I want the typographic niceties everywhere.

	(cuss line-spacing 0.1)


# Ease of use


## Startup

I want a minimal screen when I start Emacs.  Based on the beauty of configs like [Nicolas Rougier's](https://github.com/rougier/elegant-emacs) [splash screen](https://github.com/rougier/emacs-splash) [experiments](https://github.com/rougier/nano-emacs), I might try my hand at some kind of splash screen or dashboard &#x2013; but until then, a simple "Hi there!" will suffice 😎

	(cuss inhibit-startup-buffer-menu t)
	(cuss inhibit-startup-screen t)
	(cuss initial-buffer-choice t)
	(cuss initial-scratch-message ";; Hi there!\n")


## Completing-read niceties

`completing-read` is Emacs's selection-narrowing-slash-completion framework thing.  There's a bunch of packages for it, including `ido`, `icomplete`, `ivy`, and `helm`.  I use raxod52's `selectrum` and others, which *extend* without *clobbering* existing Emacs functionality.  Plus they seem to run faster, at least on Windows.


### [selectrum](https://github.com/raxod502/selectrum)

`selectrum` is the basic *sorting and selecting items from a list* functionality.  It's a drop-in replacement for `ido` or the really basic tab-completion Emacs has for, say, `find-file`.

	(use-package selectrum
	  :config
	  (selectrum-mode 1))


### [prescient](https://github.com/raxod502/prescient.el)

`prescient` helps `selectrum` be more intelligent about sorting the candidates in a list &#x2013; it's in charge of the *filtering and sorting* bit of `completing-read` and friends.  It has an algorithm that works well enough for me, though I keep hearing about [orderless](https://github.com/oantolin/orderless), enough to maybe try it as well sometime.

	(use-package prescient
	  :config
	  (prescient-persist-mode 1))

	(use-package selectrum-prescient
	  :after (selectrum prescient)
	  :config
	  (selectrum-prescient-mode 1))


### [consult](https://github.com/minad/cconsult)

`consult` is the newest package I have with this setup, and it kind of brings the `selectrum` experience up to par with `ivy`'s &#x2013; it provides functions that list, say, recently used files *alongside* buffers, allow you to search lines and go to them, etc.  It seems pretty nice so far.

By the way, the [Reddit announcement thread for consult](https://www.reddit.com/r/emacs/comments/k3c0u7) has a great comment by the author detailing [the differences between different completing-read implementations](https://www.reddit.com/r/emacs/comments/k3c0u7/consult_counselswiper_alternative_for/ge460z3/) that actually is what convinced me to try `consult`.

	(use-package consult
	  :after (selectrum)
	  :straight (consult
    	     :host github
    	     :repo "minad/consult")
	  :bind (("C-x b" . consult-buffer)
    	 ("C-x 4 b" . consult-buffer-other-window)
    	 ("C-x 5 b" . consult-buffer-other-frame)
    	 ("M-g o" . consult-outline)
    	 ("M-g l" . consult-line)
    	 ("M-y" . consult-yank-pop)
    	 ("<help> a" . consult-apropos))
	  :init
	  (fset 'multi-occur #'consult-multi-occur)
	  (consult-annotate-mode)
	  :config
	  (setf (alist-get 'execute-extended-command consult-annotate-alist)
    	#'consult-annotate-command-full))


### Ignore case

I don't like holding the Shift key if I can help it.

	(cuss completion-ignore-case t)
	(cuss read-buffer-completion-ignore-case t)
	(cuss read-file-name-completion-ignore-case t)


## [ctrlf](https://github.com/raxod502/ctrlf)

The biggest reason I use this over the default functionality of `C-s` is that `ctrlf-forward-*` wraps the search around by default.

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


## [which-key](https://github.com/justbur/emacs-which-key)

This package is really helpful for discovering functionality.  When I get more adept in my Emacs-fu, I might remove this.

	(use-package which-key
	  :custom
	  (which-key-popup-type 'minibuffer)
	  :config
	  (which-key-mode))


## Miscellaneous settings

Maybe a better title for this section is **Other settings** &#x2013; or maybe I should put them somewhere else entirely.


### Set `view-mode` when in a read-only file

`view-mode` gives easy-to-use keybindings, like Space for page-down, etc., which are nice to have when you can't edit the file anyway.

	(cuss view-read-only t)


### Don't use dialog boxen

	(cuss use-dialog-box nil)


### Enable all functions

By default, Emacs disables some commands, because NeWbIeS wOuLd GeT cOnFuSeD or some ish.  I just want to use the dang editor!

	(cuss disabled-command-function nil)


### Shorter confirmations

Instead of making me type *yes* or *no*, just let me hit the *y* or *n* key.

	(fset 'yes-or-no-p #'y-or-n-p)


### Uniquify buffer names

This names buffers with the same basename (e.g., `~/.config/emacs/config.org` and `~/.emacs.d/config.org`) in a better way than the default (`config.org<1>`, etc).

	(require 'uniquify)
	(cuss uniquify-buffer-name-style 'forward)


### Show buffer boundaries

These little L-shaped graphics at the top and bottom of buffers don't do anything, but I like 'em.

	(cuss indicate-buffer-boundaries
		  '((top . right)
    	(bottom . right)
    	(t . nil)))


### Hippie expand

At some point, will probably replace with [company](https://company-mode.github.io/).

	(global-set-key (kbd "M-/") 'hippie-expand)


### "[better defaults](https://git.sr.ht/~technomancy/better-defaults/tree/master/better-defaults.el)"

Most of these come from technomancy's repo, linked above, just copy-pasted into here.

	(cuss save-interprogram-paste-before-kill t)
	(cuss apropos-do-all t)
	(cuss mouse-yank-at-point t)
	(cuss require-final-newline t)
	(cuss visible-bell (not (string= (system-name) "larry")))
	(cuss ediff-window-setup-function #'ediff-setup-windows-plain)

1.  Zap-up-to-char, not zap-to-char

	Similarly to `ibuffer`, this is a Better default™.

		(autoload 'zap-up-to-char "misc"
		  "Kill up to, but not including, ARGth occurrence of CHAR." t)

		(global-set-key (kbd "M-z") 'zap-up-to-char)

2.  iBuffer

	A Better Default™ for `C-x C-b`.  I don't really use this, but everyone says it's worth it, so it's there.

		(global-set-key (kbd "C-x C-b") 'ibuffer)


### So-long-mode

I figure, why not go ahead and make Emacs deal with really long lines better?  Can't hurt, right?

	(if (boundp 'global-so-long-mode)
		(global-so-long-mode))


### Change `just-one-space` to `cycle-space`

I keep forgetting to actually *use* this keybind (I think it's `M-SPC`?), but cycling spacing seems *way* more useful than the default `just-one-space` function.

	(defun acdw/cycle-spacing-1 ()
	  (interactive)
	  (cycle-spacing -1))

	(bind-key [remap just-one-space] #'acdw/cycle-spacing-1)


# Persistence

Honestly, persistence across sessions was one of the best things about my well-tuned Vim setup.  Here's where I try to repeat that with Emacs.


## Auto-saves with [super-save](https://github.com/bbatsov/super-save)

The default `auto-save` functionality isn't &#x2026; *enough* for me.  I want to *actually* save the files, and I don't care about `#file#` stuff.  So &#x2026; I use this package.

	(use-package super-save
	  :custom
	  (auto-save-default nil)
	  (super-save-exclue '(".gpg"))
	  :config
	  (super-save-mode 1))


## Backup files

To be honest, I probably don't need backup files at all.  At some point, I will probably delete this.

	(cuss backup-directory-alist
		  `((".*" . ,(no-littering-expand-var-file-name "backup/"))))

	(cuss backup-by-copying 1)
	(cuss delete-old-versions -1)
	(cuss version-control t)
	(cuss vc-make-backup-files t)


## Recent files

Since I apparently *only* edit my `config.org`, this is also probably not necessary &#x2013; I'd be better off just adding a `(find-file (concat (user-emacs-directory "config.org")))` at the end 😎

But until then, it's really nice to have a `recentf` list.

	(require 'recentf)

	(add-to-list 'recentf-exclude
    	     '(no-littering-var-directory
    	       no-littering-etc-directory))

	(cuss recentf-max-menu-items 100)
	(cuss recentf-max-saved-items 100)

	(recentf-mode 1)


### Easily navigate recent files

Now I'm going through this, I might not need this function any more.  I'll have to see how `consult` goes.

	(defun recentf-find-file ()
	  "Find a recent file using `completing-read'."
	  (interactive)
	  (let ((file (completing-read "Recent file: " recentf-list nil t)))
		(when file
		  (find-file file))))

	(bind-key "C-x C-r" #'recentf-find-file)


## Save places in visited files

	(require 'saveplace)

	(cuss save-place-file (no-littering-expand-var-file-name "places"))

	(cuss save-place-forget-unreadable-files
		  (not (eq system-type 'windows-nt)))

	(save-place-mode 1)


## Save history

	(require 'savehist)

	(cuss savehist-additional-variables
		  '(kill-ring
    	search-ring
    	regexp-search-ring))

	(cuss savehist-save-minibuffer-history t)

	(cuss history-length t)

	(cuss history-delete-duplicates t)

	(savehist-mode 1)


## Undo: [undo-fu-session](https://gitlab.com/ideasman42/emacs-undo-fu-session)

The other Killer Feature of Neovim when I used it was the perisistent undo.  I *think* this works the same.  Honestly, undo is giving me a little grief recently; I need to look into it.

Note to self: if I *do* switch away from `undo-fu`, look at [undohist](https://github.com/emacsorphanage/undohist).

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

I just want to use UTF-8 everywhere, and end all files with UNIX line endings (`^J`, or `LF`).  Hell, even Windows Notepad correctly reads UNIX files nowadays (though of course you can't configure it to *save* the files in UNIX-mode).  However, since Emacs is ~40 years old, it has a billion different ways to set encodings.  This is my best attempt at setting everything up how I want it.

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


## [undo-fu](https://gitlab.com/ideasman42/emacs-undo-fu)

I've heard that Emacs' undo is weird, so here I am, trying to make it &#x2026;. *less* weird.  I keep forgetting I've installed this though, so I might uninstall it at some point.

	(use-package undo-fu
	  :bind
	  ("C-/" . undo-fu-only-undo)
	  ("C-?" . undo-fu-only-redo))


## Find/replace: [visual-regexp](https://github.com/benma/visual-regexp.el)

Another replacement for a Killer Feature in Neovim &#x2013; the ease of regexp find/replace was so wonderful, because I could easily see *what* I'd be changing with a `%s` command, as well as *how* it'd change.  This works&#x2026; pretty similarly.  It could be a little better.

	(use-package visual-regexp
	  :bind
	  ("C-c r" . 'vr/replace)
	  ("C-c q" . 'vr/query-replace))


## Visual editing


### [volatile-highlights](https://github.com/k-talo/volatile-highlights.el)

Highlights text changed by certain operations.

	(use-package volatile-highlights
	  :config
	  (volatile-highlights-mode 1))


### [expand-region](https://github.com/magnars/expand-region.el)

I don't use this a *ton*, but not because it's not useful &#x2013; I just forget it's there sometimes.

Basically, it allows you to do like a Kakoune-style incremental widening of the selection by semantic units.

	(use-package expand-region
	  :bind
	  ("C-=" . er/expand-region)
	  ("C-+" . er/contract-region))


## Clean up white space on save

I'm not sure if I'll *keep* this forever, because in combination with `super-save` I lose the final "thinking spaces" when I shift contexts to another window.

	(add-hook 'before-save-hook #'whitespace-cleanup)
	(add-hook 'before-save-hook #'delete-trailing-whitespace)


## Automatically revert a file to what it is on disk

Revert a buffer to reflect what's on disk if it's changed outside of Emacs.

	(global-auto-revert-mode 1)


# Writing

Configurations related to writing prose or verse.


## Word count: [wc-mode](https://github.com/bnbeckwith/wc-mode)

	(use-package wc-mode
	  :config
	  (rm/whitelist-add "WC")
	  :hook text-mode)


## [visual-fill-column-mode](https://github.com/joostkremers/visual-fill-column)

Center the text part of the frame within a `fill-column`-sized area in the frame as a whole.

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


### Fix mouse bindings

In `visual-fill-column-mode`, mouse bindings on the margins don't work.  In fact, they don't work when *not* in `visual-fill-column-mode`.  Let's bind those bindings.

	(dolist (vec '([left-margin wheel-down]
    	       [left-margin mouse-5]
    	       [right-margin wheel-down]
    	       [right-margin mouse-5]))
	  (bind-key vec 'scroll-down-command))

	(dolist (vec '([left-margin wheel-up]
    	       [left-margin mouse-4]
    	       [right-margin wheel-up]
    	       [right-margin mouse-4]))
	  (bind-key vec 'scroll-up-command))


## [org-mode](https://orgmode.org/)

Pretty self-explanatory, I think&#x2026;

I need to break this config up and like, comment it better.

	(use-package org
	  :custom
	  (org-startup-indented t)
	  (org-src-tab-acts-natively t)
	  (org-hide-emphasis-markers t)
	  (org-fontify-done-headline t)
	  (org-fontify-whole-heading-line t)
	  (org-fontify-quote-and-verse-blocks t)
	  (org-hide-leading-stars t)
	  (org-hidden-keywords '(author date title))
	  (org-src-window-setup 'current-window)
	  (org-pretty-entities t)
	  (org-ellipsis " ⋯ "))


### Make bullets look like centered dots

from [zzamboni.org](https://zzamboni.org/post/beautifying-org-mode-in-emacs/)

	(font-lock-add-keywords
	 'org-mode
	 '(("^ *\\([-+]\\) "
		(0 (prog1 ()
    	 (compose-region (match-beginning 1)
    			 (match-end 1)
    			 "•"))))))


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

The Other Thing Emacs is Good For.


## Formatting


### Indenting: [aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode)

This automagically indents code on every change, as opposed to `electric-indent-mode`, which only does when I like, hit `RET` or whatever.  As such, I can turn `electric-indent-mode` off.

	(use-package aggressive-indent
	  :init
	  (electric-indent-mode -1)
	  :config
	  (global-aggressive-indent-mode 1))


### [Smart tabs](https://github.com/jcsalomon/smarttabs)

I really want to like, use tabs all the time.  But I thought the `smart-tabs` package author made some good points about using tabs for semantic indentation, and spaces for the rest.  So.

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

By default, I think `prettify-symbols-mode` only changes `lambda` to `λ`.  I should, at some point, add some prettified symbols.

	(add-hook 'prog-mode-hook #'prettify-symbols-mode)


### Parentheses and frens

1.  `show-paren-style`

	A `mixed` `show-paren-style` means that, when both parentheses are visible, it just highlights them.  If one is *not*, though, it highlights the entire block.

		(cuss show-paren-style 'mixed)
		(show-paren-mode 1)

2.  [smartparens](https://github.com/Fuco1/smartparens)

	Automagically close pairs and stuff.  See also [ParEdit](https://www.emacswiki.org/emacs/ParEdit) &#x2013; maybe test that one?

		(use-package smartparens
		  :init
		  (defun acdw/setup-smartparens ()
			(require 'smartparens-config)
			(smartparens-mode 1))
		  :hook
		  (prog-mode . acdw/setup-smartparens))

3.  [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)

	Show different pairs of delimiters in diffferent colors.  Pretty!  Useful!

		(use-package rainbow-delimiters
		  :hook (prog-mode . rainbow-delimiters-mode))


### [rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html)

Show different colors *in that color*.  Useful!  Pretty!

	(use-package rainbow-mode
	  :custom
	  (rainbow-x-colors nil)
	  :hook prog-mode)


### Line numbers

I only want line numbers in `prog-mode`-derived modes.  In addition, apparently `linum-mode` works better in TUI, but is slower that `display-line-numbers`.  So I want to do some logic to see what to use.

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


## Programming languages

These are the programming languages I (don't really) use.


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
	  :mode "\\.go\\'"
	  :hook
	  (before-save . gofmt-before-save))


# Applications

Of course, the real reason we love emacs is for the application layer.  What is it they say?

> Emacs is a great operating system, lacking only a decent editor.

Yeah, that's it 😎


## Git: [magit](https://magit.vc/)

The magical porcelain.

	(use-package magit
	  :bind
	  ("C-x g" . magit-status)
	  :custom-update
	  (magit-no-confirm '(stage-all-changes))
	  :config
	  (add-hook 'magit-process-find-password-functions
    	    #'magit-process-password-auth-source))


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


## Dired

I'm still figuring out what all I can do with `dired`.

	(with-eval-after-load 'dired
	  (cuss dired-dwim-target t)
	  (cuss dired-listing-switches "-alDh")

	  (cuss wdired-allow-to-change-permissions t)
	  (bind-key "C-c w" #'wdired-change-to-wdired-mode 'dired-mode-map))


### dired-subtree

Part of the [dired-hacks](https://github.com/Fuco1/dired-hacks) package.

	(use-package dired-subtree
	  :bind (:map dired-mode-map
    	      (("i" . dired-subtree-insert)
    	       (";" . dired-subtree-remove))))


## Proced

The process editor.

	(defun acdw/setup-proced ()
	  (variable-pitch-mode -1)
	  (toggle-truncate-lines 1)
	  (proced-toggle-auto-update 1))

	(add-hook 'proced-mode-hook #'acdw/setup-proced)


## Gemini (and gopher)


### [elpher](https://thelambdalab.xyz/elpher/)

Actually, `elpher` is the reason I started using Emacs.  So thanks, smol web denizens!

Fun fact: these packages are *also* why I use `straight.el`, since they're none of them on GitHub.

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


### [gemini-mode](https://git.carcosa.net/jmcbray/gemini.el)

A major mode for `text/gemini` files.  I've changed the headings to match Elpher's.

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
	  (gemini-mode . acdw/setup-gemini-mode))


### [gemini-write](https://alexschroeder.ch/cgit/gemini-write/about/)

Alex Schroeder's Emacs implementation of the Titan protocol.  This is why I use his Gemini server, [Phoebe](https://alexschroeder.ch/cgit/phoebe/)!

	(use-package gemini-write
	  :straight (gemini-write
    	     :repo "https://alexschroeder.ch/cgit/gemini-write")
	  :config
	  (when (boundp 'acdw-secrets/elpher-gemini-tokens)
		(dolist (token acdw-secrets/elpher-gemini-tokens)
		  (add-to-list 'elpher-gemini-tokens token))))


### [post-to-gemlog-blue](https://git.sr.ht/~acdw/post-to-gemlog-blue.el)

My first (!) Emacs package, to allow posting to [gemlog.blue's web interface](https://gemlog.blue).  I don't use gemlog.blue any more, but if I didn't have this package, no one would 😎

	(use-package post-to-gemlog-blue
	  :straight (post-to-gemlog-blue
    	     :repo "https://git.sr.ht/~acdw/post-to-gemlog-blue.el"))


## Pastebin: [0x0](https://git.sr.ht/~zge/nullpointer-emacs)

Pastebins are so useful.  Now I can use them from Emacs.

	(use-package 0x0
	  :custom
	  (0x0-default-service 'ttm))


## [mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)

I've just recently started (again) using mu4e.  We'll see how it goes.

	(when (executable-find "mu")
	  (add-to-list 'load-path
    	       "/usr/share/emacs/site-lisp/mu4e")
	  (require 'mu4e)

	  (cuss mail-user-agent 'mu4e-user-agent)

	  (cuss mu4e-headers-skip-duplicates t)
	  (cuss mu4e-view-show-images t)
	  (cuss mu4e-view-show-addresses t)
	  (cuss mu4e-compose-format-flowed t)
	  (cuss mu4e-change-filenames-when-moving t)
	  (cuss mu4e-attachments-dir "~/Downloads")

	  (cuss mu4e-maildir "~/.mail/fastmail")
	  (cuss mu4e-refile-folder "/Archive")
	  (cuss mu4e-sent-folder "/Sent")
	  (cuss mu4e-drafts-folder "/Drafts")
	  (cuss mu4e-trash-folder "/Trash")

	  (fset 'my-move-to-trash "mTrash")
	  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
	  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

	  (cuss message-send-mail-function 'smtpmail-send-it)
	  (cuss smtpmail-default-smtp-server "smtp.fastmail.com")
	  (cuss smtpmail-smtp-server "smtp.fastmail.com")
	  (cuss smtpmail-stream-type 'ssl)
	  (cuss smtpmail-smtp-service 465)
	  (cuss smtpmail-local-domain "acdw.net")
	  (cuss mu4e-compose-signature
    	"Best,\nCase\n")

	  (cuss mu4e-get-mail-command "mbsync -a")
	  (cuss mu4e-update-interval 300)

	  (cuss mu4e-completing-read-function 'completing-read)
	  (cuss message-kill-buffer-on-exit t)
	  (cuss mu4e-confirm-quit nil)

	  (cuss mu4e-bookmarks
    	'((
    	   :name "Unread"
    	   :query
    	   "flag:unread AND NOT flag:trashed AND NOT maildir:/Spam"
    	   :key ?u)
    	  (
    	   :name "Today"
    	   :query "date:today..now and not maildir:/Spam"
    	   :key ?t)
    	  (
    	   :name "This week"
    	   :query "date:7d..now and not maildir:/Spam"
    	   :hide-unread t
    	   :key ?w)))

	  (cuss mu4e-headers-fields
    	'((:human-date . 12)
    	  (:flags . 6)
    	  (:mailing-list . 10)
    	  (:from-or-to . 22)
    	  (:subject)))

	  (defun acdw/setup-mu4e-view-mode ()
		(visual-fill-column-mode))

	  (add-hook 'mu4e-view-mode-hook #'acdw/setup-mu4e-view-mode))

	;; not sure about this...
	(use-package mu4e-dashboard
	  :straight (mu4e-dashboard
    	     :host github
    	     :repo "rougier/mu4e-dashboard"
    	     :branch "main"))


# Appendix A: Scripts


## `emacsdc`

Here's a wrapper script that'll start `emacs --daemon` if there isn't one, and then launche `emacsclient` on the arguments.  I'd recommend installing with `ln -s emacsdc ~/.local/bin/` or something.  Then you can set it as your `$EDITOR`!

if ! emacsclient -nc "$@" 2>/dev/null; then
        emacs --daemon
        emacsclient -nc "$@"
    fi


# Appendix B: areas for further research

-   [ebuku](https://github.com/flexibeast/ebuku) (of course, I'd need [buku](https://github.com/jarun/buku) as well) &#x2013; bookmarks
-   [KeePassXC as Secret Service](https://www.billdietrich.me/Authentication.html?expandall=1#KeePassXCandSecretService)
-   [Ignoramus](https://github.com/rolandwalker/ignoramus) &#x2013; this might not e necessary
-   [Dynamic fonts](https://github.com/rolandwalker/dynamic-fonts) &#x2013; take a look @ this and compare with my fonts section
-   [Simple clipboard integration](https://github.com/rolandwalker/simpleclip) &#x2013; test with Windows, maybe
-   [visible mark](https://git.sr.ht/~iank/visible-mark) &#x2013; show where the marks are &#x2026;
-   consider this Reddit thread: [speeding up magit](https://www.reddit.com/r/emacs/comments/k3xfa1/speeding_up_magit/)
