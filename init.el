;; init.el -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(defvar old-file-name-handler file-name-handler-alist)
(setq file-name-handler-alist nil)

(let* ((conf (expand-file-name "config"
			       user-emacs-directory))
       (conf-el (concat conf ".el"))
       (conf-org (concat conf ".org")))
  (unless (and (file-newer-than-file-p conf-el conf-org)
	       (load conf 'no-error))
    (require 'org)
    (org-babel-load-file conf-org)))

(setq gc-cons-threshold 16777216 ; 16mb
      gc-cons-percentage 0.1
      file-name-handler-alist old-file-name-handler)
