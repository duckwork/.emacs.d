;;; init.el -*- lexical-binding: t; coding: utf-8; fill-column: 70 -*-
;;; Commentary:
;; I /was/ going to convert this to org-mode, but then I found this
;; page: https://yiufung.net/post/pure-emacs-lisp-init-skeleton/,
;; which pointed out that I could use `outline-minor-mode' (or in my case,
;; `outshine') to fold and navigate a pure-elisp `init.el'.  So that's
;; what I'm doing.

;;; Custom Functions
(defun acdw/split-and-follow-window-below ()
  "Split the window below and switch to the split."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun acdw/split-and-follow-window-right ()
  "Split the window right and switch to the split."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun acdw/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun acdw/stop-gc ()
  "Stop garbage collection by setting it to a very high number."
  (setq gc-cons-threshold most-positive-fixnum))

(defun acdw/start-gc ()
  "Start garbage collection by resetting it to `*acdw/gc-cons*'."
  (setq gc-cons-threshold *acdw/gc-cons*))

(defun post-to-gemlog-blue (post-title user pass)
  "Post current buffer to gemlog.blue."
  (interactive
   (let* ((title-maybe (progn ;; TODO this is ... clunky
                         (goto-char (point-min))
                         (if (re-search-forward "^# \\(.*\\)" nil t)
                             (buffer-substring-no-properties
                              (match-beginning 1)
                              (match-end 1))
                           "")))
          (title (read-string
                  (format "Title%s: "
                          (if (string= "" title-maybe)
                              ""
                            (concat " (" title-maybe ")")))
                  nil nil title-maybe))
          (user (read-string "User: " nil))
          (pass (read-passwd "Pass: " nil)))
     (list title user pass)))

  (require 'mm-url)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mm-url-encode-www-form-urlencoded
          `(("title" . ,post-title)
            ("gemloguser" . ,user)
            ("pw" . ,pass)
            ("post" . ,(buffer-string))))))
    (with-current-buffer
        (url-retrieve-synchronously "https://gemlog.blue/post.php")
      (goto-char (point-min))
      (re-search-forward "\\(gemini://.*\\.gmi\\)")
      (elpher-go (match-string 1)))))

;;; Basic emacs config & built-in packages
;;;; /Really/ basic emacs config
(use-package calendar
  :straight nil
  :custom
  (calendar-location-name "Baton Rouge, LA")
  (calendar-latitude 30.39)
  (calendar-longitude -91.83))

(use-package browse-url
  :straight nil
  :custom
  (browse-url-browser-function 'browse-url-firefox)
  (browse-url-new-window-flag t)
  (browse-url-firefox-new-window-is-tab t))

(use-package paren
  :straight nil
  :custom
  (show-paren-style 'mixed)
  :hook
  (after-init-hook . show-paren-mode))

(use-package simple
  :straight nil
  :custom
  (save-interprogram-paste-before-kill
   t "Save existing clipboard text into killring before replacing it.")
  :hook
  (after-init-hook . global-visual-line-mode))

(use-package delsel
  :straight nil
  :hook
  (after-init-hook . delete-selection-mode))

(use-package emacs
  :straight nil
  :demand t
  :custom
  ;; completion
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; etc.
  (indent-tabs-mode nil "Indentation won't insert tabs.")
  (visible-bell (not *acdw/at-larry*) "Don't ring a bell, unless at larry.")
  (use-dialog-box nil "Ask questions in the modeline.")
  (disabled-command-function nil)
  (mark-even-if-inactive nil "Don't use the mark when inactive.")
  ;; paragraphs
  (sentence-end-double-space t "Sentences end with two spaces.")
  ;; cursor
  (cursor-type 'bar)
  (cursor-in-non-selected-windows 'hollow)
  (default-frame-alist '((tool-bar-lines . 0)
                         (menu-bar-lines . 0)
                         (vertical-scroll-bars . nil)
                         (horizontal-scroll-bars . nil)
                         (right-divider-width . 2)
                         (bottom-divider-width . 2)
                         (left-fringe-width . 2)
                         (right-fringe-width . 2)))
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-screen t)
  (initial-buffer-choice t "Start out in *scratch*.")
  (initial-scratch-message nil)
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (blink-cursor-mode 0)
  :bind
  ("C-z" . nil)
  ("C-x k" . acdw/kill-this-buffer)
  ("C-x K" . kill-buffer)
  :hook
  (prog-mode-hook . prettify-symbols-mode)
  (before-save-hook . delete-trailing-whitespace)
  (minibuffer-setup-hook . acdw/stop-gc)
  (minibuffer-exit-hook . acdw/start-gc))

(use-package display-line-numbers
  :straight nil
  :when (and (fboundp 'display-line-numbers-mode)
             (display-graphic-p))
  :hook
  (prog-mode-hook . display-line-numbers-mode))

(use-package linum
  :straight nil
  :unless  (and (fboundp 'display-line-numbers-mode)
                (display-graphic-p))
  :hook
  (prog-mode-hook . linum-mode))

(use-package mule
  :straight nil
  :init
  (set-charset-priority 'unicode)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  :custom
  (default-buffer-file-coding-system 'utf-8)
  (default-process-coding-system '(utf-8-unix . utf-8-unix))
  (locale-coding-system 'utf-8))

(use-package mouse
  :straight nil
  :custom
  (mouse-yank-at-point t "Yank at point instead of click."))

(use-package files
  :straight nil
  :custom
  (require-final-newline t)
  (confirm-kill-processes nil)
  (confirm-kill-emacs nil)
  :bind
  ("C-x f" . find-file))

(use-package window
  :straight nil
  :bind
  ([remap split-window-below] . acdw/split-and-follow-window-below)
  ([remap split-window-right] . acdw/split-and-follow-window-right))

;;;; Keep .emacs.d clean
;; load this early for other packages to use
(use-package no-littering
  :demand
  :custom
  (create-lockfiles nil)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;;;; Uniquily name buffers
(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward))

;;;; Use async when possible
(use-package async
  :config ;; not sure when this is loaded
  (dired-async-mode))

;;;; Autocompile elisp files (like this one)
(use-package auto-compile
  :custom
  (load-prefer-newer t)
  :init
  (defun acdw/setup-auto-compile ()
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))
  :hook
  (emacs-lisp-mode-hook . acdw/setup-auto-compile))

;;;; Recent files
(use-package recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :hook
  (after-init-hook . recentf-mode))

;;;; Save places in files
(use-package saveplace
  :custom
  (save-place-file (no-littering-expand-var-file-name "places"))
  (save-place-forget-unreadable-files (not *acdw/at-work*))
  :hook
  (after-init-hook . save-place-mode))

;;;; Save history of commands, etc.
(use-package savehist
  :custom
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring))
  :hook
  (after-init-hook . savehist-mode))

;;;; Authority sources for logins
;; TODO: use gpg
(use-package auth-source
  :init
  (setq auth-sources '("~/.authinfo"))
  (setq user-full-name "Case Duckworth")
  (setq user-mail-address "acdw@acdw.net"))

;;; General-ish Packages
;;;; General improvements
;;;;; Better auto-save
(use-package super-save
  :custom
  (auto-save-default nil)
  (super-save-auto-save-when-idle t)
  (super-save-exclude '(".gpg"))
  :hook
  (after-init-hook . super-save-mode))
;;;;; Restart emacs /from within/ emacs
(use-package restart-emacs)

;;;; User interface
;;;;; Pop-up help for keys
(use-package which-key
  :custom
  (which-key-enable-extended-define-key t)
  :hook
  (after-init-hook . which-key-mode))

;;;;; A better help buffer
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command))

;;;;; A better `outline-mode'
(use-package outshine
  :custom
  (outshine-cycle-emulate-tab t)
  :bind (:map outshine-mode-map
              ("<S-iso-lefttab>" . outshine-cycle-buffer)
              ("<backtab>" . outshine-cycle-buffer))
  :hook
  (emacs-lisp-mode-hook . outshine-mode))

;;;;; Item selection & narrowing
(use-package selectrum
  :custom
  (enable-recursive-minibuffers t)
  :init
  (minibuffer-depth-indicate-mode)
  :hook
  (after-init-hook . selectrum-mode))

(use-package prescient
  :hook
  (after-init-hook . prescient-persist-mode))

(use-package selectrum-prescient
  :hook
  (after-init-hook . (selectrum-prescient-mode)))

;;;;; Searching
(use-package ctrlf
  :custom
  (ctrlf-show-match-count-at-eol nil)
  :hook
  (after-init-hook . ctrlf-mode))

;;;;; Visually switch windows
(use-package ctrlxo
  :straight (ctrlxo
             :host github
             :repo "muffinmad/emacs-ctrlxo")
  :bind
  ([remap other-window] . ctrlxo))

;;;;; Undo-Fu
(use-package undo-fu
  :bind
  ("C-z" . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo))

(use-package undo-fu-session
  :after no-littering
  :custom
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'"
     "/git-rebase-todo\\'"))
  (undo-fu-session-directory
   (no-littering-expand-var-file-name "undos/"))
  :hook
  (after-init-hook . global-undo-fu-session-mode))

;;;; Theming, looks, fonts
;;;;; Fonts
;; https://github.com/kaushalmodi/.emacs.d/blob/master/init.el#L376
;; modi/font-check
(defun acdw/setup-fonts ()
  (let* ((fixed-pitch-sans-serif-family
          (cond ((x-list-fonts "Fira Code") '(:family "Fira Code"))
                ((x-list-fonts "Consolas") '(:family "Consolas"))
                ((x-list-fonts "DejaVu Sans Mono") '(:family "DejaVu Sans Mono"))
                ((x-list-fonts "Fixed") '(:family "Fixed"))
                (nil (warn "Can't find a good fixed pitch sans-serif font."))))
         (fixed-pitch-serif-family
          (cond ((x-list-fonts "Go Mono") '(:family "Go Mono"))
                ((x-list-fonts "Courier Prime") '(:family "Courier Prime"))
                ((x-list-fonts "Courier New") '(:family "Courier New"))
                ((x-list-fonts "Courier") '(:family "Courier"))
                (nil (warn "Can't find a good fixed pitch serif font."))))
         (variable-pitch-sans-serif-family
          (cond ((x-list-fonts "DejaVu Sans") '(:family "DejaVu Sans"))
                ((x-list-fonts "Go") '(:family "Go"))
                ((x-list-fonts "Arial") '(:family "Arial"))
                (nil (warn "Cant't find a good variable pitch sans-serif font."))))
         (variable-pitch-serif-family
          (cond ((x-list-fonts "DejaVu Serif") '(:family "DejaVu Serif"))
                ((x-list-fonts "Georgia") '(:family "Georgia"))
                ((x-list-fonts "Times New Roman") '(:family "Times New Roman"))
                ((x-list-fonts "Times") '(:family "Times"))
                (nil (warn "Can't find a good variable pitch serif font."))))

         (fixed-pitch-family fixed-pitch-sans-serif-family)
         (variable-pitch-family variable-pitch-serif-family)
         (default-family fixed-pitch-family))
    (custom-theme-set-faces
     'user
     `(fixed-pitch ((t (,@fixed-pitch-family))))
     `(fixed-pitch-serif ((t (,@fixed-pitch-serif-family))))
     `(variable-pitch ((t (,@variable-pitch-family))))
     `(default ((t (,@default-family))))))
  (remove-hook 'focus-in-hook #'acdw/setup-fonts))
(add-hook 'focus-in-hook #'acdw/setup-fonts)

;;;;; Modeline
(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-enable-word-count t)
  :hook
  (after-init-hook . doom-modeline-mode))

;;;;; Ligatures
(use-package ligature
  :straight (ligature
	     :host github
	     :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode
			  '("++" "--" "/=" "&&" "||" "||="
			    "->" "=>" "::" "__"
			    "==" "===" "!=" "=/=" "!=="
			    "<=" ">=" "<=>"
			    "/*" "*/" "//" "///"
			    "\\n" "\\\\"
			    "<<" "<<<" "<<=" ">>" ">>>" ">>="
			    "|=" "^="
			    "**" "--" "---" "----" "-----"
			    "==" "===" "====" "====="
			    "</" "<!--" "</>" "-->" "/>"
			    ":=" "..." ":>" ":<" ">:" "<:"
			    "::=" ;; add others here
			    ))
  :hook
  (after-init-hook . global-ligature-mode))

;;;;; Unicode
(use-package unicode-fonts
  :hook
  (after-init-hook . unicode-fonts-setup))

;;;;; Modus themes
(use-package modus-operandi-theme
  :config
  (load-theme 'modus-operandi t t)

  (defun acdw/sunrise ()
    (enable-theme 'modus-operandi))

  (if *acdw/at-work*
      (enable-theme 'modus-operandi)
    (run-at-time (nth 1 (split-string (sunrise-sunset)))
                 (* 60 60 24) #'acdw/sunrise)))

(when *acdw/at-home*
  (use-package modus-vivendi-theme
    :config
    (load-theme 'modus-vivendi t t)

    (defun acdw/sunset ()
      (enable-theme 'modus-vivendi))

    (run-at-time (nth 4 (split-string (sunrise-sunset)))
                 (* 60 60 24) #'acdw/sunset)
    (run-at-time "12am" (* 60 60 24) #'acdw/sunset)))

;;;;; Convert ^L to a line
(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

;;;; General text editing
;;;;; Jump to characters fast
(use-package avy
  :bind
  ("C-/" . avy-goto-char-timer))

;;;;; Show text commands acted on
(use-package volatile-highlights
  :hook
  (after-init-hook . volatile-highlights-mode))

;;;;; Visual replacement for `zap-to-char'
(use-package zop-to-char
  :bind
  ([remap zap-to-char] . zop-to-char)
  ([remap zap-up-to-char] . zop-up-to-char))

;;;;; Kill & mark things more visually
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

;;;;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook
  (after-init-hook . whole-line-or-region-global-mode))

;;;;; Expand region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;;;; Programming
;;;;; Code completion
(use-package company
  :custom
  (company-idle-delay 0.1)
  (company-show-numbers t)
  :config
  (let ((map company-active-map))
    (mapc (lambda (x)
            (define-key map (format "%d" x)
              `(lambda ()
                 (interactive)
                 (company-complete-number ,x))))
          (number-sequence 0 9)))
  :hook
  (prog-mode-hook . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package company-quickhelp
  :hook
  (company-mode-hook . company-quickhelp-local-mode))

(use-package company-prescient
  :hook
  (company-mode-hook . company-prescient-mode))

;;;;; Git integration
(use-package magit
  :when *acdw/at-home*
  :bind
  ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; use libgit to speed up magit, only when cmake is available
(when (executable-find "cmake")
  (use-package libgit)

  (use-package magit-libgit
    :after (magit libgit))
  )

(use-package forge
  :when *acdw/at-home*
  :after magit
  :config
  (setq forge-owned-accounts '(("duckwork"))))

;;;;; Code formatting & display
;;;;;; Keep code properly indented
(use-package aggressive-indent
  :hook
  (prog-mode-hook . aggressive-indent-mode))

(use-package format-all
  :hook
  (prog-mode-hook . format-all-mode))

;;;;;; Smartly deal with pairs
(use-package smartparens
  :init
  (defun acdw/setup-smartparens ()
    (require 'smartparens-config)
    (smartparens-mode))
  :hook
  (prog-mode-hook . acdw/setup-smartparens))

;;;;;; Show delimiters as different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

;;;;;; Show colors as they appear in the buffer
(use-package rainbow-mode
  :hook
  (prog-mode-hook . rainbow-mode))

;;;; Writing
;;;;; `fill-column', but in `visual-line-mode'
(use-package visual-fill-column
  :custom
  (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  (visual-fill-column-center-text t)
  :config
  (advice-add 'text-scale-adjust
              :after #'visual-fill-column-adjust))

;;;; Machine-specific
;;;;; Linux at home
(when *acdw/at-home*
;;;;;; Edit files with `sudo' (I think?)
  (use-package su
    :hook
    (after-init-hook . su-mode))

;;;;;; Implement XDG Trash specification
  (use-package trashed
    :custom
    (delete-by-moving-to-trash t))

;;;;;; Build exec-path from $PATH
  (use-package exec-path-from-shell
    :demand
    :config
    (exec-path-from-shell-initialize))
  )

;;; Specialized packages
;;;; Gemini & Gopher
(use-package elpher
  :straight (elpher
	     :repo "git://thelambdalab.xyz/elpher.git")
  :bind (:map elpher-mode-map
	      ("n" . elpher-next-link)
	      ("p" . elpher-prev-link)
              ("o" . elpher-follow-current-link)
              ("G" . elpher-go-current))
  :init
  (defun acdw/setup-elpher ()
    (variable-pitch-mode)
    (set-fill-column 100)
    (visual-fill-column-mode))
  :hook (elpher-mode-hook . acdw/setup-elpher))

(use-package gemini-mode
  :straight (gemini-mode
	     :repo "https://git.carcosa.net/jmcbray/gemini.el.git")
  :init
  (defun acdw/setup-gemini-mode ()
    (set-fill-column 100)
    (visual-fill-column-mode))
  :hook (gemini-mode-hook . acdw/setup-gemini-mode))

(use-package gemini-write
  :straight (gemini-write
	     :repo "https://alexschroeder.ch/cgit/gemini-write"))

;;;; org-mode
(use-package org
  :custom
  (org-startup-indented t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  :config
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-+*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1)
                                                          (match-end 1)
                                                          "•"))))))
  :hook
  (org-mode-hook . variable-pitch-mode))

(use-package org-bullets
  :hook
  (org-mode-hook . org-bullets-mode))

;;;; SLIME -- for LISP
(use-package slime
  :custom
  (inferior-lisp-program (cond ((executable-find "sbcl")
                                (executable-find "sbcl")))))

;;;; Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

;;;; Fennel
(use-package fennel-mode
  :mode "\\.fnl\\'")

;;;; Web-mode
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  :mode (("\\.ts\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :init
  (defun acdw/setup-web-mode ()
    (set (make-local-variable 'company-backends
                              '(company-css
                                company-web-html
                                company-files))))
  :hook
  (web-mode-hook . acdw/setup-web-mode))

(use-package emmet-mode
  :init
  (defun acdw/setup-emmet-in-web-mode ()
    (let ((web-mode-cur-language
           (web-mode-language-at-pos)))
      (if (string= web-mode-cur-language "css")
          (setq emmet-use-css-transform t)
        (setq emmet-use-css-transform nil))))
  :hook
  (web-mode-hook . emmet-mode)
  (web-mode-before-auto-complete-hooks . acdw/setup-emmet-in-web-mode))

     (provide 'init)
;;; init.el ends here
