;; init.el -*- lexical-binding: t -*-

(setq load-prefer-newer t)

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
    ;; A plain require here just loads the older `org' in Emacs' install dir.  We
    ;; need to add the newer one to the `load-path', hopefully that's all.
    (add-to-list 'load-path (expand-file-name "straight/build/org/"))
    (require 'org)
    (org-babel-load-file conf-org)))
