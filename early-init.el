;; early-init.el -*- no-byte-compile: t; -*-
;; This file is automatically tangled from config.org.
;; Hand edits will be overwritten!

;;  Disable loading of =package.el=

;; I use =straight.el= instead.


;; [[file:config.org::*Disable loading of =package.el=][Disable loading of =package.el=:1]]
(setq package-enable-at-startup nil)
;; Disable loading of =package.el=:1 ends here

;; Don't resize the frame when loading fonts


;; [[file:config.org::*Don't resize the frame when loading fonts][Don't resize the frame when loading fonts:1]]
(setq frame-inhibit-implied-resize t)
;; Don't resize the frame when loading fonts:1 ends here

;; Resize frame by pixels


;; [[file:config.org::*Resize frame by pixels][Resize frame by pixels:1]]
(setq frame-resize-pixelwise t)
;; Resize frame by pixels:1 ends here

;; Shoe-horned from elsewhere in =config.org=

;; A fundamental tension of literal programming is logical versus
;; programmatic ordering.  I understand that's a problem it's meant to
;; solve but hey, maybe I'm not quite there yet.  I feel that having this
;; weird shoe-horning of other bits of my config here, in a backwater
;; heading in an appendix, isn't quite the future I wanted.  But it's
;; what I have for now.


;; [[file:config.org::*Shoe-horned from elsewhere in =config.org=][Shoe-horned from elsewhere in =config.org=:1]]
(add-to-list 'default-frame-alist
	     '(tool-bar-lines . 0))

(tool-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(menu-bar-lines . 0))

(menu-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil)
	     '(horizontal-scroll-bars . nil))

(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
;; Shoe-horned from elsewhere in =config.org=:1 ends here
