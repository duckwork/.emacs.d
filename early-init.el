;;; early-init.el ~ acdw

;;; this needs to happen first -- speed up init
(setq gc-cons-threshold most-positive-fixnum)
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq message-log-max 16384)
(setq byte-compile-warnings
  '(not free-vars unresolved noruntime lexical make-local))

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)
            (setq gc-cons-threshold (* 32 1024 1024)))
          t)

;;(setq debug-on-error t)

;;; different platforms
(defconst *acdw/at-work* (eq system-type 'windows-nt))
(defconst *acdw/at-larry* (string= (system-name) "larry"))
(defconst *acdw/at-bax* (string= (system-name) "bax"))
(defconst *acdw/at-home* (or *acdw/at-larry* *acdw/at-bax*))

;; this needs to be before bootstrapping straight.el
(when *acdw/at-work*
  (add-to-list 'exec-path "~/bin")
  (add-to-list 'exec-path "C:/Users/aduckworth/Downloads/PortableGit/bin"))

;;; gui
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)
(setq initial-scratch-message nil)

;;; straight.el ~ github.com/raxod502/straight.el

(setq straight-use-package-by-default t) ; use use-package
(setq use-package-hook-name-suffix nil) ; don't assume -hook

;; bootstrap
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

;; install use-package with straight
(straight-use-package 'use-package)
