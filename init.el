;;; init.el -*- lexical-binding: t -*-
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

(setq-default load-prefer-newer t)

(let* (;; Speed up init
       (gc-cons-threshold most-positive-fixnum)
       (file-name-handler-alist nil)
       ;; Config file names
       (config (expand-file-name "config"
                                 user-emacs-directory))
       (config.el (concat config ".el"))
       (config.org (concat config ".org"))
       (straight-org-dir (locate-user-emacs-file "straight/build/org")))
  ;; Okay, let's figure this out.
  ;; `and' evaluates each form, and returns nil on the first that
  ;; returns nil.  `unless' only executes its body if the test
  ;; returns nil.  So.
  ;; 1. Test if config.org is newer than config.el.  If it is (t), we
  ;;    *want* to evaluate the body, so we need to negate that test.
  ;; 2. Try to load the config.  If it errors (nil), it'll bubble that
  ;;    to the `and' and the body will be evaluated.
  (unless (and (not (file-newer-than-file-p config.org config.el))
               (load config :noerror))
    ;; A plain require here just loads the older `org'
    ;; in Emacs' install dir.  We need to add the newer
    ;; one to the `load-path', hopefully that's all.
    (when (file-exists-p straight-org-dir)
      (add-to-list 'load-path straight-org-dir))
    ;; Load config.org
    (require 'org)
    (org-babel-load-file config.org)))

;;; init.el ends here
