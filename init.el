;; init.el ~ acdw

(server-start)
(setq custom-file (concat user-emacs-directory "custom.el"))

;;; bootstrap packages
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(unless package--initialized (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package use-package
  :ensure
  :config
  (require 'use-package))

(use-package quelpa
  :ensure)

(use-package quelpa-use-package
  :ensure)

;;; packages
;; use-package helpers
(use-package use-package-ensure-system-package
  :ensure)

;; emacs
(use-package emacs
  :custom
  (user-full-name "Case Duckworth")
  (user-mail-address "acdw@acdw.net")
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (visible-bell nil)
  (frame-title-format
   '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
              "%b"))))
  (show-paren-style 'mixed)
  (backup-directory-alist
   `((".*" . ,(concat user-emacs-directory "saves/"))))
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 10)
  (version-control t)
  (auto-save-file-name-transforms
   `((".*" ,(concat user-emacs-directory "saves/") t)))
  (create-lockfiles nil)
  (scroll-conservatively 100)
  (calendar-location-name "Baton Rouge, LA")
  (calendar-latitude 30.39)
  (calendar-longitude -91.83)
  (show-paren-style 'mixed)
  (save-place-file (expand-file-name "places" user-emacs-directory))
  :config
  (blink-cursor-mode 0)
  (delete-selection-mode 1)
  (global-auto-revert-mode t)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-prettify-symbols-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1)
  (show-paren-mode 1)
  (global-visual-line-mode 1)
  (save-place-mode 1)
  :hook
  ((beforpe-save . delete-trailing-whitespace)
   (prog-mode . (lambda ()
                  (if (and (fboundp 'display-line-numbers-mode)
                           (display-graphic-p))
                      #'display-line-numbers-mode
                    #'linum-mode))))
  :bind
  (("C-z" . nil)
   ("M-1" . delete-other-windows)
   ("M-o" . mode-line-other-buffer)))

;; async
(use-package async
  :ensure)

;; delight
(use-package delight
  :ensure)

;; startup buffer
(use-package dashboard
  :ensure
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

;; modeline
(use-package all-the-icons
  :ensure
  :config
  (set-face-attribute 'mode-line nil :height 100)
  (set-face-attribute 'mode-line-inactive nil :height 100)
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package doom-modeline
  :ensure
  :custom
  (doom-modeline-height 16)
  (doom-modeline-enable-word-count t)
  (doom-modeline-mu4e t)
  (doom-modeline-gnus nil)
  (doom-modeline-irc t)
  :config
  (display-time-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (display-battery-mode 1)
  (doom-modeline-mode t))

;; themes
(use-package modus-operandi-theme
  :if window-system
  :ensure
  :custom
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-3d-modeline t)
  :config
  (load-theme 'modus-operandi t t)
  (run-at-time (nth 1 (split-string (sunrise-sunset)))
	       (* 60 60 24)
	       (lambda () (enable-theme 'modus-operandi))))

(use-package modus-vivendi-theme
  :if window-system
  :ensure
  :custom
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-bold-constructs t)
  (modus-vivendi-theme-3d-modeline t)
  :config
  (load-theme 'modus-vivendi t t)
  (run-at-time (nth 4 (split-string (sunrise-sunset)))
	       (* 60 60 24)
	       (lambda () (enable-theme 'modus-vivendi))))

;; minibuffer completion
(use-package ivy
  :ensure
  :delight
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure
  :delight
  :bind ("C-s" . swiper-isearch))

(use-package counsel
  :ensure
  :delight
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :ensure
  :after (ivy)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package savehist
  :ensure
  :config
  (savehist-mode 1))

(use-package ivy-dired-history
  :after (ivy)
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

;; completion
(use-package company
  :ensure
  :delight
  :custom
  (company-idle-delay 0.2)
  :hook
  (after-init . global-company-mode))

;; undo
(use-package undo-tree
  :ensure
  :config
  (global-undo-tree-mode))

;; git
(use-package magit
  :ensure
  :custom
  (magit-push-always-verify nil)
  (git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

;; irc
(defun my/fetch-password (&rest params)
  "fetch a password from auth-sources"
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
	(let ((secret (plist-get match :secret)))
	  (if (functionp secret)
	      (funcall secret)
	    secret))
      (error "Password not found for %S" params))))

(defun my/sasl-password (nick server)
  "fetch an sasl password for $server"
  (my/fetch-password :user nick :host server))

(use-package circe
  :ensure
  :custom
  (circe-network-options
   `(("Freenode"
      :tls t
      :port 6697
      :nick "acdw"
      :sasl-username "acdw"
      :sasl-password ,(my/sasl-password "acdw" "irc.freenode.net")
      :channels ("#emacs" "#emacs-circe" "#daydreams"))
     ("Tilde.chat"
      :tls t
      :port 6697
      :nick "acdw"
      :sasl-username "acdw"
      :sasl-password ,(my/sasl-password "acdw" "irc.tilde.chat")
      :channels ("#gemini" "#meta"))))
  :custom-face
  (circe-my-message-face ((t (:foreground "dark violet")))))
      

;; mail
(use-package mu4e
  :ensure-system-package mu ; TODO ensure mu4e is also installed
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :init
  (require 'smtpmail-async)
  :custom
  (mu4e-headers-skip-duplicates t)
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses t)
  (mu4e-compose-format-flowed t)
  (mu4e-date-format "%Y-%m-%d")
  (mu4e-headers-date-format "%Y-%m-%d")
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachments-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-update-interval (* 60 60))
  (mu4e-maildir "~/Mail/fastmail")
  (mu4e-refile-folder "/Archive")
  (mu4e-sent-folder "/Sent")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder "/Trash")
  (message-send-mail-function 'async-smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.fastmail.com")
  (smtpmail-smtp-server "smtp.fastmail.com")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  :config
  (fset 'my-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash))

;; gemini/gopher
(use-package elpher
  :quelpa
  (elpher
    :fetcher git
    :url "git://thelambdalab.xyz/elpher.git"))

(use-package visual-fill-column
  :ensure)

(use-package gemini-mode
  :after visual-fill-column
  :quelpa
  (gemini-mode
    :fetcher git
    :url "https://git.carcosa.net/jmcbray/gemini.el.git"))

(use-package gemini-write
  :after gemini-mode
  :quelpa
  (gemini-write
    :fetcher git
    :url "https://alexschroeder.ch/cgit/gemini-write"))

;; window manager
(use-package exwm
  :ensure
  :demand
  :custom
  (exwm-layout-show-all-buffers t)
  (mouse-autoselect-window t)
  (focus-follows-mouse t)
  (exwm-workspace-number 1)
  (exwm-input-global-keys
	`(
	  ([?\s-r] . exwm-reset)
	  ([?\s-w] . exwm-workspace-switch)
	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))
  (exwm-input-simulation-keys
	'(([?\C-b] . [left])
	  ([?\C-f] . [right])
	  ([?\C-p] . [up])
	  ([?\C-n] . [down])
	  ([?\C-a] . [home])
	  ([?\C-e] . [end])
	  ([?\M-v] . [prior])
	  ([?\C-v] . [next])
	  ([?\C-d] . [delete])
	  ([?\C-k] . [S-end delete])))
  :hook
  ((exwm-update-class .
		      (lambda () "Rename buffer to window's class name"
			(exwm-workspace-rename-buffer exwm-class-name)))
   (exwm-update-title .
		      (lambda () "Update workspace name to window title"
			(when (not exwm-instance-name)
			  (exwm-workspace-rename-buffer exwm-title))))
   (exwm-manage-finish .
		       (lambda () "Disable simulation keys for Firefox"
			 (when (and exwm-class-name
				    (string= exwm-class-name "Firefox"))
			   (exwm-input-set-local-simulation-keys nil))))
   (exwm-init .
	      (lambda () "Autostart"
		(start-process-shell-command "cmst" nil "cmst -m -w 5")
		(start-process-shell-command "keepassxc" nil "keepassxc")
		(start-process-shell-command "pa-applet" nil
					     "pa-applet --disable-key-grabbing --disable-notifications")
		(start-process-shell-command "picom" nil "picom"))))
  :config
  (exwm-enable)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

(use-package exwm-mff
  :ensure
  :hook (exwm-init . exwm-mff-mode))

(use-package exwm-edit
  :ensure)

(use-package desktop-environment
  :ensure
  :hook (exwm-init . desktop-environment-mode)
  :custom
  (desktop-environment-update-exwm-global-keys :global)
  (desktop-environment-brightness-get-command "light")
  (desktop-environment-brightness-set-command "light %s")
  (desktop-environment-brightness-get-regexp "^\\([0-9]+\\)")
  (desktop-environment-brightness-normal-increment "-A 10")
  (desktop-environment-brightness-normal-decrement "-U 10")
  (desktop-environment-brightness-small-increment "-A 5")
  (desktop-environment-brightness-small-decrement "-U 5")
  (desktop-environment-volume-get-command "pavolume get")
  (desktop-environment-volume-set-command "pavolume %s")
  (desktop-environment-volume-toggle-command "pavolume mute toggle")
  (desktop-environment-volume-get-regexp "^\\([0-9]+\\)")
  (desktop-environment-volume-normal-increment "inc 10")
  (desktop-environment-volume-normal-decrement "dec 10")
  (desktop-environment-volume-small-increment "inc 5")
  (desktop-environment-volume-small-decrement "dec 5"))
