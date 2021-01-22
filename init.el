;; init.el -*- lexical-binding: t -*-
;; This file is automatically tangled from config.org.
;; Hand edits will be overwritten!

(setq-default load-prefer-newer t)

(defmacro when-at (conditions &rest commands)
  "Run COMMANDS, or let the user know, when at a specific place.

CONDITIONS are one of `:work', `:home', or a list beginning with
those and other conditions to check.  COMMANDS are only run if
all CONDITIONS are met.

If COMMANDS is empty or nil, simply return the result of CONDITIONS."
  (declare (indent 1))
  (let ((at-work '(memq system-type '(ms-dos windows-nt)))
        (at-home '(memq system-type '(gnu gnu/linux gnu/kfreebsd))))
    (pcase conditions
      (:work (if commands `(when ,at-work ,@commands) at-work))
      (:home (if commands `(when ,at-home ,@commands) at-home))
      ((guard (eq (car conditions) :work))
       (if commands
           `(when (and ,at-work ,@(cdr conditions))
              ,@commands)
         `(and ,at-work ,@(cdr conditions))))
      ((guard (eq (car conditions) :home))
       (if commands
           `(when (and ,at-home ,@(cdr conditions))
              ,@commands)
         `(and ,at-work ,@(cdr conditions)))))))

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
