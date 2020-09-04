;;; early-init.el ~ acdw

;;; different platforms
(setq my/is-windows-p (eq system-type 'windows-nt)
      my/is-linux-p (eq system-type 'gnu/linux)
      ;; TODO my/is-larry-p, my/is-bax-p (hostname)
      )

(when my/is-windows-p
  (setenv "PATH" (concat "path/to/git" ";" (getenv "PATH"))))


;;; gui
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)

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

;;; straight.el ~ github.com/raxod502/straight.el

;; use use-package
(setq straight-use-package-by-default t)

;; boostrap straight.el
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

;; install use-package with straight
(straight-use-package 'use-package)

;;; other init stuff

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
