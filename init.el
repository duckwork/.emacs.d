;; init.el ~ acdw -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 256 1024 1024))
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq message-log-max 16384)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
;; post-init
(add-hook 'after-init-hook
	  (lambda ()
	    (setq file-name-handler-alist file-name-handler-alist-old)
	    (setq gc-cons-threshold (* 32 1024 1024)))
	  t)

(unless (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(scroll-bar-mode -1)
(fringe-mode '(7 . 1))

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "acdw")
(setq initial-buffer-choice t)
(setq initial-scratch-message nil)

(server-start)

(setq confirm-kill-processes nil)

(setq user-full-name "Case Duckworth")
(setq user-mail-address "acdw@acdw.net")
(setq calendar-location-name "Baton Rouge, LA")
(setq calendar-latitude 30.39)
(setq calendar-longitude -91.83)

(setq show-paren-style 'mixed)
(show-paren-mode 1)

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "saves/"))))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 4)
(setq version-control t)

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "saves/") t)))
(auto-save-mode 1)

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
	  (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)
(add-hook 'focus-out-hook 'full-auto-save) ;; this might be resource intensive?

(setq save-place-file (expand-file-name "places" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(blink-cursor-mode 0)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode t)

(save-place-mode 1)

(global-visual-line-mode 1)
(delight 'global-visual-line-mode)

(display-time-mode 1)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook (lambda ()
			    (if (and (fboundp 'display-line-numbers-mode)
				     (display-graphic-p))
				#'display-line-numbers-mode
			      #'linum-mode)))

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
  :ensure
  :custom
  (quelpa-git-clone-depth nil))

(use-package quelpa-use-package
  :ensure)

;;; packages
;; use-package helpers
(use-package use-package-ensure-system-package
  :ensure)

(use-package delight
  :ensure)

;; exwm
(use-package exwm
  :ensure
  :demand
  :custom
  (exwm-layout-show-all-buffers t)
  (mouse-autoselect-window t)
  (exwm-workspace-number 4)
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
     ([?\M-b] . [C-left])
     ([?\C-f] . [right])
     ([?\M-f] . [C-right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\M-v] . [prior])
     ([?\C-v] . [next])
     ([?\C-d] . [delete])
     ([?\C-k] . [S-end delete])
     ([?\C-s] . [?\C-f])
     ([?\C-w] . [?\C-x])
     ([?\M-w] . [?\C-c])
     ([?\C-y] . [?\C-v])))
  :hook
  ((exwm-update-class .
		      (lambda () "Rename buffer to window's class name"
			(exwm-workspace-rename-buffer exwm-class-name)))
   (exwm-update-title .
		      (lambda () "Update workspace name to window title"
			(when (not exwm-instance-name)
			  (exwm-workspace-rename-buffer exwm-title))))
   (exwm-init .
	      (lambda () "Autostart"
		(start-process-shell-command "cmst" nil "cmst -m -w 5")
		(start-process-shell-command "keepassxc" nil "keepassxc")
		(start-process-shell-command
		 "pa-applet" nil
		 "pa-applet --disable-key-grabbing --disable-notifications")
		(start-process-shell-command "cbatticon" nil "cbatticon"))))
  :config
  (require 'exwm)
  (exwm-enable)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

;; (use-package mini-modeline
;;   :quelpa (mini-modeline
;; 	   :repo "kiennq/emacs-mini-modeline"
;; 	   :fetcher github)
;;   :delight
;;   :custom
;;   (mini-modeline-enhance-viusual t)
;;   (mini-modeline-display-gui-line t)
;;   (mini-modeline-right-padding 7) ;; characters -- for systemtray
;;   (mini-modeline-face-attr nil)
;;   :config
;;   (mini-modeline-mode t))

(use-package desktop-environment
  :ensure
  :delight
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

(use-package trashed
  :ensure
  :custom
  (delete-by-moving-to-trash t))

(use-package switch-window
  :ensure
  :custom
  (switch-window-shortcut-style 'qwerty)
  :bind
  ("M-o" . switch-window))

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
   (doom-modeline-icon nil)
   (doom-modeline-enable-word-count t)
   (doom-modeline-mu4e t)
   (doom-modeline-gnus nil)
   (doom-modeline-irc t)
   :custom-face
   (doom-modeline-vspc-face ((t (:inherit nil))))
   :config
   (doom-modeline-mode t))

(use-package zoom
  :ensure
  :delight
  :custom
  (zoom-size '(0.618 . 0.618)))

;; themes
(defun my/sunrise ()
  (enable-theme 'modus-operandi)
  (desktop-environment-brightness-set 60))

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
	       (* 60 60 24) #'my/sunrise))

(defun my/sunset ()
  (enable-theme 'modus-vivendi)
  (desktop-environment-brightness-set 35))

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
	       (* 60 60 24) #'my/sunset)
  (run-at-time "12am" (* 60 60 24) #'my/sunset))

;; sudo
(use-package su
  :ensure
  :config
  (su-mode 1))

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

(use-package magit
  :ensure
  :bind
  ("M-g" . magit))

;; mu4e
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

;; try packages out
(use-package try
  :ensure)

;; vterm babeee
(use-package vterm
  :ensure)
