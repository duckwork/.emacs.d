;; early-init.el -*- no-byte-compile: t; -*-
;; This file is automatically tangled from config.org.
;; Hand edits will be overwritten!

(setq package-enable-at-startup nil)
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
	    "raxod502/straight.el/develop/install.el")
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
(unless (ignore-errors (acdw/bootstrap-straight))
  (let ((msg "Straight.el didn't bootstrap correctly.  Cloning directly"))
    (message "%s..." msg)
    (call-process "git" nil
		  (get-buffer-create "*bootstrap-straight-messages*") nil
		  "clone"
		  "https://github.com/raxod502/straight.el"
		  (expand-file-name "straight/repos/straight.el"
				    user-emacs-directory))
    (message "%s...Done." msg)
    (acdw/bootstrap-straight)))

(setq-default frame-inhibit-implied-resize t)

(setq-default frame-resize-pixelwise t)

(add-to-list 'default-frame-alist
	     '(tool-bar-lines . 0))

(tool-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(menu-bar-lines . 0))

(menu-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil)
	     '(horizontal-scroll-bars . nil))

(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
