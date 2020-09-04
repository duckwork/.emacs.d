;; init.el ~ acdw -*- lexical-binding: t -*-

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

(display-time-mode 1)
(setq display-time-format "%R")

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook
	  (if (and (fboundp 'display-line-numbers-mode)
				     (display-graphic-p))
				#'display-line-numbers-mode
			      #'linum-mode))

(setq uniquify-buffer-name-style 'forward)

(set-face-attribute 'default nil :font "GoMono Nerd Font-11")

(mouse-avoidance-mode 'jump)

;;; Packages

(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))

(use-package async
  :init (dired-async-mode 1))

;; exwm
(use-package exwm
  :demand
  :custom
  (exwm-layout-show-all-buffers t)
  ;;(mouse-autoselect-window t)
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
   (exwm-init . window-divider-mode)
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

;; (use-package exwm-firefox-core
;;   :after exwm
;;   :straight (exwm-firefox-core
;; 	     :type git
;; 	     :host github
;; 	     :repo "walseb/exwm-firefox-core"))

;; (use-package exwm-firefox
;;   :after exwm-firefox-core
;;   :straight (exwm-firefox
;; 	     :type git
;; 	     :host github
;; 	     :repo "ieure/exwm-firefox")
;;   :config
;;   (exwm-firefox-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(use-package exwm-mff
  :after exwm
  :hook
  (exwm-init . exwm-mff-mode))

(use-package desktop-environment
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
  :custom
  (delete-by-moving-to-trash t))

(use-package switch-window
  :custom
   (switch-window-shortcut-style 'qwerty)
  :bind
  ([remap other-window] . switch-window)
  ("s-o" . switch-window))

(defun split-and-follow-below ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key [remap split-window-below] 'split-and-follow-below)

(defun split-and-follow-right ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key [remap split-window-right] 'split-and-follow-right)

;; modeline
(use-package doom-modeline
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
  :custom
  (zoom-size '(0.618 . 0.618)))

;; themes
(defun my/sunrise ()
  (enable-theme 'modus-operandi)
  (start-process-shell-command "light" nil "light -S 60"))

(use-package modus-operandi-theme
  :if window-system
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
  (start-process-shell-command "light" nil "light -S 35"))

(use-package modus-vivendi-theme
  :if window-system
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
  :config
  (su-mode 1))

;; minibuffer completion
(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package swiper
  :bind ("C-s" . swiper-isearch))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :after (ivy)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package magit
  :bind
  ("M-x g" . magit))

;; mu4e
(progn
  (require 'mu4e)
  (require 'mu4e-contrib)
  (require 'smtpmail-async)
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-date-format "%Y-%m-%d")
  (setq mu4e-headers-date-format "%Y-%m-%d")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-attachments-dir "~/Downloads")
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-update-interval (* 60 60))
  (setq mu4e-maildir "~/Mail/fastmail")
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-trash-folder "/Trash")
  (setq message-send-mail-function 'async-smtpmail-send-it)
  (setq smtpmail-default-smtp-server "smtp.fastmail.com")
  (setq smtpmail-smtp-server "smtp.fastmail.com")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl)
  (fset 'my-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash))

;; tramp
(setq tramp-terminal-type "tramp")

;; try packages out
;(use-package try)

;; vterm babeee
;(use-package vterm)

(use-package which-key
  :custom
  (which-key-mode 1))

(use-package company
  :custom
  (company-idle-delay 0.1)
  :hook
  (prog-mode . company-mode))

;;; gemini/gopher
(use-package elpher
  :straight
  (elpher
   :repo "git://thelambdalab.xyz/elpher.git"))

(use-package gemini-mode
  :straight
  (gemini-mode
   :repo "https://git.carcosa.net/jmcbray/gemini.el.git"))

(use-package gemini-write
  :straight
  (gemini-write
   :repo "https://alexschroeder.ch/cgit/gemini-write"))

;;; better help messages
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;;; eshell
;; ~ much from http://www.howardism.org/Technical/Emacs/eshell-fun.html
;;; TODO

(use-package avy
  :bind
  ("M-s" . avy-goto-char))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))
