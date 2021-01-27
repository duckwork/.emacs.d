;;; early-init.el -*- no-byte-compile: t; -*-
;; Copyright (C) 2020 Case Duckworth

;; Author: Case Duckworth <acdw@acdw.net>
;; Created: Sometime during the Covid-19 lockdown, 2019
;; Keywords: configuration
;; URL: https://tildegit.org/acdw/emacs

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file is automatically tangled from config.org.
;; Hand edits will be overwritten!

;;; Code:

;; BOOTSTRAP PACKAGE MANAGEMENT
(let ((win-app-dir "~/Applications"))
  (dolist (path (list
                 ;; Windows
                 (expand-file-name "exe" win-app-dir)
                 (expand-file-name "Git/bin" win-app-dir)
                 (expand-file-name "Git/usr/bin" win-app-dir)
                 (expand-file-name "Git/mingw64/bin" win-app-dir)
                 (expand-file-name "Everything" win-app-dir)
                 (expand-file-name "Win-builds/bin" win-app-dir)
                 ;; Linux
                 (expand-file-name "bin" user-emacs-directory)
                 (expand-file-name "~/bin")
                 (expand-file-name "~/.local/bin")
                 (expand-file-name "~/Scripts")
                 ))
    (when (file-exists-p path)
      (add-to-list 'exec-path path :append))))

;; Set $PATH
(setenv "PATH" (mapconcat #'identity exec-path path-separator))
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
(when (executable-find "git")
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
      (acdw/bootstrap-straight))))
;; SETUP FRAME
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
(setq-default frame-inhibit-implied-resize t
              frame-resize-pixelwise t)

;;; early-init.el ends here
