;;; init.el -*- lexical-binding: t; coding: utf-8; fill-column: 70 -*-

;;; Commentary:
;; I /was/ going to convert this to org-mode, but then I found this
;; page: https://yiufung.net/post/pure-emacs-lisp-init-skeleton/,
;; which pointed out that I could use `outline-mode' (or in my case,
;; `outshine') to fold and navigate a pure-elisp `init.el'.  So that's
;; what I'm doing.

;;; Basic emacs config & built-in packages
;;;; /Really/ basic emacs config
;; I /did/ use `better-defaults', but it turns out that that package
;; is (a) short and (b) mostly overriden by other settings.
(use-package emacs
  :demand ; make sure this stuff loads
  :init
  ;; where I am
  (setq calendar-location-name "Baton Rouge, LA")
  (setq calendar-latitude 30.39)
  (setq calendar-longitude -91.83)

  ;; firefox is love, firefox is life
  (setq browse-url-browser-function 'browse-url-firefox
        browse-url-new-window-flag t
        browse-url-firefox-new-window-is-tab t)

  ;; honestly not sure if this is necessary
  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including, ARGth occurence of CHAR." t)

  ;; show parentheses
  (setq show-paren-style 'mixed)
  (show-paren-mode)

  ;; always work on visual lines
  (global-visual-line-mode)

  ;; make the mouse avoid where I'm typing
  (mouse-avoidance-mode 'jump)

  ;; delete the selection when I start typing, like a normal editor
  (delete-selection-mode)

  ;; ignore case
  (setq-default completion-ignore-case t
                read-buffer-completion-ignore-case t
                read-file-name-completion-ignore-case t)

  ;; etc defaults
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil
                save-interprogram-paste-before-kill t
                apropos-do-all t
                mouse-yank-at-point t
                require-final-newline t
                visible-bell (not *acdw/at-larry*)
                ediff-window-setup-function 'ediff-setup-windows-plain
                use-dialog-box nil
                mark-even-if-inactive nil
                sentence-end-double-space t)

  ;; utf-8 is now, old man
  (set-charset-priority 'unicode)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8
        default-process-coding-system '(utf-8-unix . utf-8-unix)
        locale-coding-system 'utf-8)

  ;; don't confirm killing
  (setq confirm-kill-processes nil
        confirm-kill-emacs nil)

  ;; simplify the GUI
  (setq default-frame-alist '((tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (right-divider-width . 2)
                              (bottom-divider-width . 2)
                              (left-fringe-width . 2)
                              (right-fringe-width . 2))
        inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        initial-buffer-choice t
        initial-scratch-message nil)

  ;; set up the cursor
  (blink-cursor-mode 0)
  (setq-default cursor-type 'bar
                cursor-in-non-selected-windows 'hollow)

  ;; display line numbers in `prog-mode'
  (add-hook 'prog-mode-hook
            (if (and (fboundp 'display-line-numbers-mode)
                     (display-graphic-p))
                #'display-line-numbers-mode
              #'linum-mode))

  ;; custom functions
  (defun split-and-follow-below ()
    "Split the window below and switch to the split."
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun split-and-follow-right ()
    "Split the window right and switch to the split."
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (defun full-auto-save ()
    "Save all buffers that (a) have files associated and (b) are modified."
    (interactive)
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (basic-save-buffer)))))

  (defun kill-this-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer nil))

  :bind
  ("C-x C-b" . ibuffer)
  ("M-z" . zap-up-to-char)
  ([remap split-window-below] . split-and-follow-below)
  ([remap split-window-right] . split-and-follow-right)
  ("C-x f" . find-file)
  ("C-z" . nil)
  ("C-x k" . kill-this-buffer)
  ("C-x K" . kill-buffer)

  :hook
  (prog-mode-hook . prettify-symbols-mode)
  ((auto-save-hook focus-out-hook) . full-auto-save)
  (before-save-hook . delete-trailing-whitespace))

;;;; Keep .emacs.d clean
;; load this early for other packages to use
(use-package no-littering
  :demand
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

  (setq create-lockfiles nil)

  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))

  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (auto-save-mode))

;;;; Uniquily name buffers
(use-package uniquify
  :straight nil
  :init
  (setq uniquify-buffer-name-style 'forward))

;;;; Use async when possible
(use-package async
  :config
  (dired-async-mode))

;;;; Autocompile elisp files (like this one)
(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode))

;;;; Recent files
(use-package recentf
  :init
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode))

;;;; Save places in files
(use-package saveplace
  :init
  (setq save-place-file (no-littering-expand-var-file-name "places"))
  (when *acdw/at-work*
    (setq save-place-forget-unreadable-files nil))
  :config
  (save-place-mode))

;;;; Save history of commands, etc.
(use-package savehist
  :init
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  :config
  (savehist-mode))

;;;; Authority sources for logins
;; TODO: use gpg
(use-package auth-source
  :init
  (setq auth-sources '("~/.authinfo"))
  (setq user-full-name "Case Duckworth")
  (setq user-mail-address "acdw@acdw.net"))

;;; General-ish Packages
;;;; General improvements

;;;;; Diminish TODO: is this necessary?
(use-package diminish)

;;;;; Restart emacs /from within/ emacs
(use-package restart-emacs)

;;;; User interface
;;;;; Pop-up help for keys
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;;;;; A better help buffer
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command))

;;;;; A better `outline-mode'
(use-package outshine
  :init
  (setq outshine-cycle-emulate-tab t)
  :bind (:map outshine-mode-map
              ("<S-iso-lefttab>" . outshine-cycle-buffer)
              ("<backtab>" . outshine-cycle-buffer))
  :hook
  (emacs-lisp-mode-hook . outshine-mode))

;;;;; Item selection & narrowing
(use-package selectrum
  :init
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)
  :config
  (selectrum-mode))

(use-package prescient)

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

;;;;; Searching
(use-package ctrlf
  :config
  (ctrlf-mode))

;;;;; Visually switch windows
(use-package switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty)
  :bind
  ([remap other-window] . switch-window)
  ("s-o" . switch-window))

;;;; Theming, looks, fonts
;;;;; Fonts
;; I'm doing these outside of any 'package' b/c ... idk.  I want to?
(let* ((fixed-pitch-sans-serif-family
        (cond ((x-list-fonts "Fira Code") '(:family "Fira Code"))
              ((x-list-fonts "Consolas") '(:family "Consolas"))
              ((x-list-fonts "DejaVu Sans Mono") '(:family "DejaVu Sans Mono"))
              ((x-list-fonts "Fixed") '(:family "Fixed"))
              (nil (warn "Can't find a good fixed pitch sans-serif font."))))
       (fixed-pitch-serif-family
        (cond ((x-list-fonts "Go Mono") '(:family "Go Mono"))
              ((x-list-fonts "Courier Prime") '(:family "Courier Prime"))
              ((x-list-fonts "Courier New") '(:family "Courier New"))
              ((x-list-fonts "Courier") '(:family "Courier"))
              (nil (warn "Can't find a good fixed pitch serif font."))))
       (variable-pitch-sans-serif-family
        (cond ((x-list-fonts "DejaVu Sans") '(:family "DejaVu Sans"))
              ((x-list-fonts "Go") '(:family "Go"))
              ((x-list-fonts "Arial") '(:family "Arial"))
              (nil (warn "Cant't find a good variable pitch sans-serif font."))))
       (variable-pitch-serif-family
        (cond ((x-list-fonts "DejaVu Serif") '(:family "DejaVu Serif"))
              ((x-list-fonts "Georgia") '(:family "Georgia"))
              ((x-list-fonts "Times New Roman") '(:family "Times New Roman"))
              ((x-list-fonts "Times") '(:family "Times"))
              (nil (warn "Can't find a good variable pitch serif font."))))

       (fixed-pitch-family fixed-pitch-sans-serif-family)
       (variable-pitch-family variable-pitch-serif-family)
       (default-family fixed-pitch-family))
  (custom-theme-set-faces
   'user
   `(fixed-pitch ((t (,@fixed-pitch-family))))
   `(fixed-pitch-serif ((t (,@fixed-pitch-serif-family))))
   `(variable-pitch ((t (,@variable-pitch-family))))
   `(default ((t (,@default-family))))))
;;;;; Modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil
        doom-modeline-enable-word-count t)

  ;; (when *acdw/at-larry*
  ;;   (setq display-time-format "%R")
  ;;   (display-time-mode))

  :hook
  (window-setup-hook . doom-modeline-mode))

;;;;; Ligatures
(use-package ligature
  :straight (ligature
	     :host github
	     :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode
			  '("++" "--" "/=" "&&" "||" "||="
			    "->" "=>" "::" "__"
			    "==" "===" "!=" "=/=" "!=="
			    "<=" ">=" "<=>"
			    "/*" "*/" "//" "///"
			    "\\n" "\\\\"
			    "<<" "<<<" "<<=" ">>" ">>>" ">>="
			    "|=" "^="
			    "**" "--" "---" "----" "-----"
			    "==" "===" "====" "====="
			    "</" "<!--" "</>" "-->" "/>"
			    ":=" "..." ":>" ":<" ">:" "<:"
			    "::=" ;; add others here
			    ))
  (global-ligature-mode))

;;;;; Unicode
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;;;;; Modus themes
(use-package modus-operandi-theme
  :if window-system
  :config
  (load-theme 'modus-operandi t t)

  (defun acdw/sunrise ()
    (enable-theme 'modus-operandi)
    (start-process-shell-command "light" nil "light -S 60"))

  (if *acdw/at-work*
      (enable-theme 'modus-operandi)
    (run-at-time (nth 1 (split-string (sunrise-sunset)))
                 (* 60 60 24) #'acdw/sunrise)))

(when *acdw/at-home*
  (use-package modus-vivendi-theme
    :if window-system
    :config
    (load-theme 'modus-vivendi t t)

    (defun acdw/sunset ()
      (enable-theme 'modus-vivendi)
      (start-process-shell-command "light" nil "light -S 35"))

    (run-at-time (nth 4 (split-string (sunrise-sunset)))
                 (* 60 60 24) #'acdw/sunset)
    (run-at-time "12am" (* 60 60 24) #'acdw/sunset)))

;;;; General text editing

;;;;; Jump to characters fast
(use-package avy
  :bind
  ("M-s" . avy-goto-char-timer))

;;;;; Show text commands acted on
(use-package volatile-highlights
  :config
  (volatile-highlights-mode))

;;;;; Visual replacement for `zap-to-char'
(use-package zop-to-char
  :bind
  ([remap zap-to-char] . zop-to-char)
  ([remap zap-up-to-char] . zop-up-to-char))

;;;;; Kill & mark things more visually
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

;;;;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

;;;;; Expand region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;;;; Programming

;;;;; Code completion
(use-package company
  :init
  (setq company-idle-delay 0.1
        company-show-numbers t)
  :config
  (let ((map company-active-map))
    (mapc (lambda (x)
            (define-key map (format "%d" x)
              `(lambda ()
                 (interactive)
                 (company-complete-number ,x))))
          (number-sequence 0 9)))
  :hook
  (prog-mode-hook . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package company-quickhelp
  :hook
  (company-mode-hook . company-quickhelp-local-mode))

(use-package company-prescient
  :hook
  (company-mode-hook . company-prescient-mode))

;;;;; Git integration
(use-package magit
  :if *acdw/at-home*
  :bind
  ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; use libgit to speed up magit, only at home
(when (and *acdw/at-home* (executable-find "cmake"))
  (use-package libgit)

  (use-package magit-libgit
    :after (magit libgit))
  )

(use-package forge
  :if *acdw/at-home*
  :after magit
  :config
  (setq forge-owned-accounts '(("duckwork"))))

;;;;; Code formatting & display

;;;;;; Keep code properly indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook
  (prog-mode-hook . aggressive-indent-mode))

;;;;;; Smartly deal with pairs
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

;;;;;; Show delimiters as different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

;;;;;; Show colors as they appear in the buffer
(use-package rainbow-mode
  :hook
  (prog-mode-hook . rainbow-mode))

;;;; Writing

;;;;; `fill-column', but in `visual-line-mode'
(use-package visual-fill-column
  :init
  (setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  (setq visual-fill-column-center-text t)
  :config
  (advice-add 'text-scale-adjust
              :after #'visual-fill-column-adjust))

;;;; Machine-specific

;;;;; Linux at home
(when *acdw/at-home*

;;;;;; Edit files with `sudo' (I think?)
  (use-package su
    :config
    (su-mode))

;;;;;; Implement XDG Trash specification
  (use-package trashed
    :init
    (setq delete-by-moving-to-trash t))

;;;;;; Build exec-path from $PATH
  (use-package exec-path-from-shell
    :demand
    :config
    (exec-path-from-shell-initialize))
  )

;;; Specialized packages

;;;; Gemini & Gopher
(use-package elpher
  :straight (elpher
	     :repo "git://thelambdalab.xyz/elpher.git")
  :bind (:map elpher-mode-map
	      ("n" . elpher-next-link)
	      ("p" . elpher-prev-link)
              ("o" . elpher-follow-current-link)
              ("G" . elpher-go-current))
  :hook (elpher-mode-hook . (lambda ()
                              (variable-pitch-mode)
                              (set-fill-column 100)
                              (visual-fill-column-mode))))

(use-package gemini-mode
  :straight (gemini-mode
	     :repo "https://git.carcosa.net/jmcbray/gemini.el.git"))

(use-package gemini-write
  :straight (gemini-write
	     :repo "https://alexschroeder.ch/cgit/gemini-write"))

(defun post-to-gemlog-blue (post-title user pass)
  "Post current buffer to gemlog.blue."
  (interactive
   (let* ((title-maybe (progn ;; TODO this is ... clunky
                         (goto-char (point-min))
                         (if (re-search-forward "^# \\(.*\\)" nil t)
                             (buffer-substring-no-properties
                              (match-beginning 1)
                              (match-end 1))
                           "")))
          (title (read-string
                  (format "Title%s: "
                          (if (string= "" title-maybe)
                              ""
                            (concat " (" title-maybe ")")))
                  nil nil title-maybe))
          (user (read-string "User: " nil))
          (pass (read-passwd "Pass: " nil)))
     (list title user pass)))

  (require 'mm-url)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mm-url-encode-www-form-urlencoded
          `(("title" . ,post-title)
            ("gemloguser" . ,user)
            ("pw" . ,pass)
            ("post" . ,(buffer-string))))))
    (with-current-buffer
        (url-retrieve-synchronously "https://gemlog.blue/post.php")
      (goto-char (point-min))
      (re-search-forward "\\(gemini://.*\\.gmi\\)")
      (elpher-go (match-string 1)))))

;;;; exwm ~ Emacs X Window Manager
;; (when *acdw/at-larry*
;;   (use-package exwm
;;     :if window-system
;;     :demand
;;     :init
;;     (add-to-list 'default-frame-alist '(fullscreen . maximized)))
;;     :custom
;;     (exwm-layout-show-all-buffers t)
;;     (exwm-workspace-warp-cursor t)
;;     ;;(mouse-autoselect-window t)
;;     (exwm-workspace-number 4)
;;     (exwm-input-global-keys
;;      `(
;;        ([remap split-window-below] . split-and-follow-below)
;;        ([remap split-window-right] . split-and-follow-right)
;;        ([?\s-r] . exwm-reset)
;;        ([?\s-w] . exwm-workspace-switch)
;;        ([?\s-&] . (lambda (command)
;; 		    (interactive (list (read-shell-command "$ ")))
;; 		    (start-process-shell-command command nil command)))
;;        ,@(mapcar (lambda (i)
;; 		   `(,(kbd (format "s-%d" i)) .
;; 		     (lambda ()
;; 		       (interactive)
;; 		       (exwm-workspace-switch-create ,i))))
;; 	         (number-sequence 0 9))))
;;     (exwm-input-simulation-keys
;;      '(([?\C-b] . [left])
;;        ([?\M-b] . [C-left])
;;        ([?\C-f] . [right])
;;        ([?\M-f] . [C-right])
;;        ([?\C-p] . [up])
;;        ([?\C-n] . [down])
;;        ([?\C-a] . [home])
;;        ([?\C-e] . [end])
;;        ([?\M-v] . [prior])
;;        ([?\C-v] . [next])
;;        ([?\C-d] . [delete])
;;        ([?\C-k] . [S-end delete])
;;        ([?\C-s] . [?\C-f])
;;        ([?\C-w] . [?\C-x])
;;        ([?\M-w] . [?\C-c])
;;        ([?\C-y] . [?\C-v])))
;;     :hook
;;     ((exwm-update-class-hook .
;; 			     (lambda () "Rename buffer to window's class name"
;; 			       (exwm-workspace-rename-buffer exwm-class-name)))
;;      (exwm-update-title-hook .
;; 			     (lambda () "Update workspace name to window title"
;; 			       (when (not exwm-instance-name)
;; 			         (exwm-workspace-rename-buffer exwm-title))))
;;      (exwm-init-hook . window-divider-mode)
;;      (exwm-init-hook .
;; 		     (lambda () "Autostart"
;; 		       (start-process-shell-command "cmst" nil "cmst -m -w 5")
;; 		       (start-process-shell-command "keepassxc" nil "keepassxc")
;; 		       (start-process-shell-command
;; 		        "pa-applet" nil
;; 		        "pa-applet --disable-key-grabbing --disable-notifications")
;; 		       (start-process-shell-command
;;                         "cbatticon" nil "cbatticon"))))
;;     :config
;;     (require 'exwm)
;;     (exwm-enable)
;;     (require 'exwm-systemtray)
;;     (exwm-systemtray-enable))

;;   (use-package exwm-firefox-core
;;     :after exwm
;;     :straight (exwm-firefox-core
;;  	       :type git
;;  	       :host github
;;  	       :repo "walseb/exwm-firefox-core"))

;;   (use-package exwm-firefox
;;     :after exwm-firefox-core
;;     :straight (exwm-firefox
;;  	       :type git
;;  	       :host github
;;  	       :repo "ieure/exwm-firefox")
;;     :config
;;     (exwm-firefox-mode))

;;   (use-package exwm-mff
;;     :straight (exwm-mff
;;                :host github
;;                :repo "ieure/exwm-mff"
;;                :fork (
;;                       :host github
;;                       :repo "duckwork/exwm-mff"))
;;     :after exwm
;;     :hook
;;     (exwm-init-hook . exwm-mff-mode))

;;   (use-package exwm-edit)

;;   ) ;; end of *acdw/at-larry* block for exwm

;;;; IRC
(use-package circe
  :if *acdw/at-larry*
  :init
  (defun my/fetch-password (&rest params)
    "Fetch a password from auth-sources"
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
	  (let ((secret (plist-get match :secret)))
	    (if (functionp secret)
	        (funcall secret)
	      secret))
        (error "Password not found for %S" params))))

  (defun my/sasl-password (nick server)
    "Fetch a password for $server and $nick"
    (my/fetch-password :user nick :host server))
  (require 'lui-autopaste)
  (defun my/circe-prompt ()
    (lui-set-prompt
     (concat (propertize (concat (buffer-name) ">")
			 'face 'circe-prompt-face)
	     " ")))
  (defun my/lui-setup ()
    (setq right-margin-width 5
	  fringes-outside-margins t
	  word-wrap t
	  wrap-prefix "             ")
    (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil))
  :hook
  (circe-channel-mode-hook . enable-lui-autopaste)
  (circe-chat-mode-hook . my/circe-prompt)
  (lui-mode-hook . my/lui-setup)
  :config
  (setq circe-default-part-message "Peace out, cub scouts")
  (setq circe-default-quit-message "See  You Space Cowpokes ......")
  (setq circe-default-realname "Case D")
  (setq circe-highlight-nick-type 'all)
  (setq circe-reduce-lurker-spam t)
  (setq circe-format-say "{nick:-12s} {body}")
  (setq circe-format-self-say "{nick:-11s}> {body}")
  (setq lui-time-stamp-position 'right-margin)
  (setq lui-fill-type nil)
  (setq lui-time-stamp-format "%H:%M")
  (setq lui-track-bar-behavior 'before-switch-to-buffer)
  (setq circe-network-options
        `(("Freenode"
           :tls t
           :port 6697
           :nick "acdw"
           :sasl-username "acdw"
           :sasl-password ,(my/sasl-password "acdw" "irc.freenode.net")
           :channels ("#emacs" "#daydreams"))
          ("Tilde.chat"
           :tls t
           :port 6697
           :nick "acdw"
           :sasl-username "acdw"
           :sasl-password ,(my/sasl-password "acdw" "irc.tilde.chat")
           :channels ("#gemini" "#meta"))))
  (enable-lui-track-bar)
  :custom-face
  (circe-my-message-face ((t (:inherit 'circe-highlight-nick-face :weight normal))))
  (circe-originator-face ((t (:weight bold))))
  (circe-prompt-face ((t (:inherit 'circe-my-message-face)))))

;;;; eshell
(use-package eshell
  :init
  (defun eshell/emacs (&rest args)
    "Open a file in emacs."
    (if (null args)
        (bury-buffer)
      (mapc #'find-file
            (mapcar #'expand-file-name
                    (eshell-flatten-list (reverse args))))))
  (defun eshell/info (&optional subject)
    "Invoke `info', optionally opening Info to SUBJECT."
    (require 'cl)
    (let ((buf (current-buffer)))
      (Info-directory)
      (if (not (null subject))
          (let ((node-exists (ignore-errors (Info-menu subject))))
            (if (not node-exists)
                (format "No menu item `%s' in node `(dir)Top'."
                        subject)))))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode))

;;;; org-mode
(use-package org
  :init
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-fontify-done-headline t)
  (setq org-hide-leading-stars t)
  (setq org-pretty-entities t)

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-+*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1)
                                                          (match-end 1)
                                                          "•"))))))
  :hook
  (org-mode-hook . variable-pitch-mode))

(use-package org-bullets
  :hook
  (org-mode-hook . (lambda () (org-bullets-mode))))

;;;; SLIME -- for LISP
(use-package slime
  :init
  (setq inferior-lisp-program (cond ((executable-find "sbcl")
                                     (executable-find "sbcl")))))

(provide 'init)
;;; init.el ends here
