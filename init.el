  ;; This file replaces itself with the actual configuration when first run.  To keep only this version in git, run this command:
  ;; git update-index --assume-unchanged init.el
  ;;
  ;; If it needs to be changed, start tracking it again thusly:
  ;; git update-index --no-assume-unchanged init.el

  (require 'org)
  (find-file (concat user-emacs-directory "config.org"))
  (org-babel-tangle)
  (load-file (concat user-emacs-directory "early-init.el"))
  (load-file (concat user-emacs-directory "init.el"))
