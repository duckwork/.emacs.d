;; init.el -*- lexical-binding: t -*-

(let* ((conf (expand-file-name "config"
			       user-emacs-directory))
       (conf-el (concat conf ".el"))
       (conf-org (concat conf ".org")))
  (unless (and (file-newer-than-file-p conf-el conf-org)
	       (load conf 'no-error))
    (require 'org)
    (org-babel-load-file conf-org)))
