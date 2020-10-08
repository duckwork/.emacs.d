;;; early-init.el ~ acdw -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; `early-init.el' is new as of Emacs 27.1.  It contains ... /early initiation/.
;; What does that mean?  Who knows.  What I /do know/ is that it runs /before/
;; `package.el' is loaded, so I can stop it from loading, since I use `straight.el'.
;; Other than that, there's some other init stuff that needs to happen as early
;; as possible -- think bootstrap-level.

;;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum)

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq message-log-max 16384)
(setq byte-compile-warnings
      '(not free-vars unresolved noruntime lexical make-local))

;;; Restore stuff after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)
            (setq gc-cons-threshold (* 32 1024 1024))
            (garbage-collect))
          t)

;; (setq debug-on-error t)

;;; Define the platforms I work on
(defconst *acdw/at-work* (eq system-type 'windows-nt))
(defconst *acdw/at-larry* (string= (system-name) "larry"))
(defconst *acdw/at-bax* (string= (system-name) "bax"))
(defconst *acdw/at-home* (or *acdw/at-larry* *acdw/at-bax*))

;;;; When at work, I have to use Portable Git.
(when *acdw/at-work*
  (add-to-list 'exec-path "~/bin")
  (add-to-list 'exec-path "C:/Users/aduckworth/Downloads/PortableGit/bin"))

;;; `straight.el' ~ github.com/raxod502/straight.el

;;;; Bootstrap
;; NOTE: this doesn't work on Windows (download straight directly)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Bootstrap `use-package'
(setq-default use-package-verbose nil
              use-package-expand-minimally t
              use-package-enable-imenu-support t
	      use-package-hook-name-suffix nil)

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
