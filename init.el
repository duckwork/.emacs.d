;; init.el -*- lexical-binding: t -*-
;; This file is automatically tangled from config.org.
;; Hand edits will be overwritten!

(setq-default load-prefer-newer t)

(let* (;; Speed up init
       (gc-cons-threshold most-positive-fixnum)
       (file-name-handler-alist nil)
       ;; Config file names
       (config (expand-file-name "config"
				 user-emacs-directory))
       (config.el (concat config ".el"))
       (config.org (concat config ".org"))
       (straight-org-dir (expand-file-name "straight/build/org"
					   user-emacs-directory)))
       ;; Unless config.org is /newer/ than config.el, *or* the config
       ;; is able to be loaded without errors, load the config from
       ;; config.org.
       (unless (or (file-newer-than-file-p config.org config.el)
		   (load config 'no-error))
	 ;; A plain require here just loads the older `org'
	 ;; in Emacs' install dir.  We need to add the newer
	 ;; one to the `load-path', hopefully that's all.
	 (when (file-exists-p straight-org-dir)
	   (add-to-list 'load-path straight-org-dir))
	 ;; Load config.org
	 (require 'org)
	 (org-babel-load-file config.org)))
