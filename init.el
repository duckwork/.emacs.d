;; init.el -*- lexical-binding: t -*-
;; This file is automatically tangled from config.org.
;; Hand edits will be overwritten!

;; Prefer newer files to older files


;; [[file:config.org::*Prefer newer files to older files][Prefer newer files to older files:1]]
(setq load-prefer-newer t)
;; Prefer newer files to older files:1 ends here

;; Load the config

;; I keep most of my config in =config.el=, which is tangled directly
;; from this file.  This init just loads that file, either from lisp or
;; directly from Org if it's newer.


;; [[file:config.org::*Load the config][Load the config:1]]
(let* (;; Speed up init
       (gc-cons-threshold most-positive-fixnum)
       (file-name-handler-alist nil)
       ;; Config file names
       (conf (expand-file-name "config"
                               user-emacs-directory))
       (conf-el (concat conf ".el"))
       (conf-org (concat conf ".org")))
  (unless (and (file-newer-than-file-p conf-el conf-org)
               (load conf 'no-error))
    ;; A plain require here just loads the older `org'
    ;; in Emacs' install dir.  We need to add the newer
    ;; one to the `load-path', hopefully that's all.
    (add-to-list 'load-path (expand-file-name "straight/build/org"
                                              user-emacs-directory))
    (require 'org)
    (org-babel-load-file conf-org)))
;; Load the config:1 ends here
