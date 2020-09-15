;;; init.el ~ acdw -*- lexical-binding: t -*-

;;; emacs configuration - general

(use-package emacs
  :demand
  :init
  (setq calendar-location-name "Baton Rouge, LA")
  (setq calendar-latitude 30.39)
  (setq calendar-longitude -91.83)

  (setq browse-url-browser-function 'browse-url-generic)
  (setq  browse-url-generic-program "firefox"))

(use-package auth-source
  :init
  (setq auth-sources '("~/.authinfo")) ;; TODO: gpg
  (setq user-full-name "Case Duckworth")
  (setq user-mail-address "acdw@acdw.net"))

(use-package better-defaults
  :demand
  :config ; add other "better defaults" of my own
  (when acdw/at-larry
    (setq visible-bell nil))

  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 4)
  (setq create-lockfiles nil)
  (setq auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups/") t)))
  (auto-save-mode)

  (defun full-auto-save ()
    (interactive)
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (basic-save-buffer)))))
  (add-hook 'auto-save-hook 'full-auto-save)
  (add-hook 'focus-out-hook 'full-auto-save) ; might be resource-intensive

  ;; follow the split!
  (defun split-and-follow-below ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun split-and-follow-right ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (setq save-place-file
        (expand-file-name "places" user-emacs-directory))
  (save-place-mode)

  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)

  (fset 'yes-or-no-p 'y-or-n-p)

  (global-visual-line-mode)
  (mouse-avoidance-mode 'jump)
  (setq show-paren-style 'mixed)

  ;; TODO figure out how to add this to the :hook block
  (add-hook 'prog-mode-hook (if (and (fboundp 'display-line-numbers-mode)
				     (display-graphic-p))
				#'display-line-numbers-mode
			      #'linum-mode))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; don't confirm death
  (setq confirm-kill-processes nil)
  (setq confirm-kill-emacs nil)

  ;; cursor betterment
  (blink-cursor-mode 0)
  (setq cursor-type 'bar)

  :bind
  ([remap split-window-below] . split-and-follow-below)
  ([remap split-window-right] . split-and-follow-right)

  :hook
  (auto-save-hook . full-auto-save)
  (focus-out-hook . full-auto-save)
  (before-save-hook . delete-trailing-whitespace))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (setq load-prefer-newer t))

;; start the server when at home

(if (not acdw/at-work)
    (server-start))

;;; quality-of-life improvements

(use-package diminish)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package recentf
  :init
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  :config
  (recentf-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package restart-emacs)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  :bind
  ("C-x b" . ivy-switch-buffer)
  ("C-c v" . ivy-push-view)
  ("C-c V" . ivy-pop-view)
  ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode))

(use-package swiper
  :bind
  ("C-s" . swiper-isearch))

(use-package counsel ; includes ivy, counsel, swiper
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("<f2> j" . counsel-set-variable)
  ("C-c c" . counsel-compile)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c L" . counsel-git-log)
  ("C-c k" . counsel-rg)
  ("C-c m" . counsel-linux-app)
  ("C-x l" . counsel-locate)
  ("C-c J" . counsel-file-jump)
  ("C-c b" . counsel-bookmark)
  ("C-c d" . counsel-descbinds)
  ("C-c g" . counsel-git)
  ("C-c o" . counsel-outline)
  ("C-c t" . counsel-load-theme)
  ("C-c F" . counsel-org-file))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode)
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line))

(use-package avy
  :bind
  ("M-s" . avy-goto-char-timer)
  :config
  (use-package ivy-avy))

;;; programming

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook
  (prog-mode-hook . aggressive-indent-mode))

(use-package magit
  :if (not acdw/at-work)
  :bind
  ("C-x g" . magit))

(use-package forge
  :if (not acdw/at-work)
  :after magit
  :custom
  (forge-owned-accounts '(("duckwork"))))

(use-package company
  :commands company-mode
  :init
  (setq company-idle-delay 0.1)
  :hook
  (prog-mode-hook . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (use-package company-quickhelp
    :hook
    (company-mode . (lambda ()
                      (company-quickhelp-local-mode)))))

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

(unless acdw/at-work
  (use-package su
    :config
    (su-mode))

  (use-package trashed
    :init
    (setq delete-by-moving-to-trash t)))

;;; writing

(use-package visual-fill-column
  :init
  (setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  (setq visual-fill-column-center-text t)
  :config
  (advice-add 'text-scale-adjust
              :after #'visual-fill-column-adjust))

;;; window management

(use-package switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty)
  :bind
  ([remap other-window] . switch-window)
  ("s-o" . switch-window))

;;; theming and looks

(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-enable-word-count t)
  (when acdw/at-larry
    (setq display-time-format "%R")
    (display-time-mode))
  :config
  (doom-modeline-mode))

(when acdw/at-larry
  (use-package statusbar
    :straight (statusbar
               :host github
               :repo "dakra/statusbar.el")
    :hook
    (exwm-init-hook . statusbar-mode)))

;; modus themes

(use-package modus-operandi-theme
  :if window-system
  :config
  (load-theme 'modus-operandi t t)

  (defun acdw/sunrise ()
    (enable-theme 'modus-operandi)
    (start-process-shell-command "light" nil "light -S 60"))

  (if acdw/at-work
      (enable-theme 'modus-operandi)
    (run-at-time (nth 1 (split-string (sunrise-sunset)))
                 (* 60 60 24) #'acdw/sunrise)))

(unless acdw/at-work
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

;;; gemini/gopher
(use-package elpher
  :straight (elpher
	     :repo "git://thelambdalab.xyz/elpher.git")
  :bind (:map elpher-mode-map
	      ("n" . 'elpher-next-link)
	      ("p" . 'elpher-prev-link)))

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
                             (concat " ("
                                     (buffer-substring-no-properties
                                      (match-beginning 1)
                                      (match-end 1))
                                     ")")
                           "")))
          (title (read-string
                  (format "Title%s: " title-maybe)
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

;;; exwm ~ Emacs X Window Manager
(when acdw/at-larry
  (use-package exwm
    :if window-system
    :demand
    :custom
    (exwm-layout-show-all-buffers t)
    (exwm-workspace-warp-cursor t)
    ;;(mouse-autoselect-window t)
    (exwm-workspace-number 4)
    (exwm-input-global-keys
     `(
       ([remap split-window-below] . split-and-follow-below)
       ([remap split-window-right] . split-and-follow-right)
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
    ((exwm-update-class-hook .
			     (lambda () "Rename buffer to window's class name"
			       (exwm-workspace-rename-buffer exwm-class-name)))
     (exwm-update-title-hook .
			     (lambda () "Update workspace name to window title"
			       (when (not exwm-instance-name)
			         (exwm-workspace-rename-buffer exwm-title))))
     (exwm-init-hook . window-divider-mode)
     (exwm-init-hook .
		     (lambda () "Autostart"
		       (start-process-shell-command "cmst" nil "cmst -m -w 5")
		       (start-process-shell-command "keepassxc" nil "keepassxc")
		       (start-process-shell-command
		        "pa-applet" nil
		        "pa-applet --disable-key-grabbing --disable-notifications")
		       (start-process-shell-command
                        "cbatticon" nil "cbatticon"))))
    :config
    (require 'exwm)
    (exwm-enable)
    (require 'exwm-systemtray)
    (exwm-systemtray-enable))

  (use-package exwm-firefox-core
    :after exwm
    :straight (exwm-firefox-core
 	       :type git
 	       :host github
 	       :repo "walseb/exwm-firefox-core"))

  (use-package exwm-firefox
    :after exwm-firefox-core
    :straight (exwm-firefox
 	       :type git
 	       :host github
 	       :repo "ieure/exwm-firefox")
    :config
    (exwm-firefox-mode))

  (use-package exwm-mff
    :straight (exwm-mff
               :host github
               :repo "duckwork/exwm-mff")
    :after exwm
    :hook
    (exwm-init-hook . exwm-mff-mode))

  (use-package exwm-edit)

  )
;; end of acdw/at-larry block for exwm
