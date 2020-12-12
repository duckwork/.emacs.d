;; init.el -*- lexical-binding: t -*-

(let ((conf (expand-file-name "config"
			      user-emacs-directory)))
  (unless (load conf 'no-error)
    (require 'org)
    (org-babel-load-file (concat conf ".org"))))
