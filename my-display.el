;;; my-display --- Changes to how Emacs looks
;;
;; Copyright (C) 2014 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;;; Commentary:
;;
;; Mostly we are disabling elements but there is some eye-candy here
;;
;;; Code:

;; Require prerequisites

(require 'use-package)
(require 'my-vars)
(require 'my-libs)

;;; Display hacks
;
; With --daemon mode using start-up evaluation is no longer useful as
; the .emacs will almost certainly run without window-system meaning
; anything. Instead we should do things on the fly as new frames are
; created.


;; Disable the menu and tool bars, they just take up space.
(use-package menu-bar
  :commands menu-bar-mode
  :init (menu-bar-mode -1))

(use-package tool-bar
  :commands tool-bar-mode
  :init (tool-bar-mode -1))

(use-package scroll-bar
  :commands scroll-bar-mode
  :init (scroll-bar-mode -1))

;; Use imagemagick if we have it to view images
(use-package image
  :if (getenv "DISPLAY")
  :commands imagemagick-register-types
  :init (imagemagick-register-types))

(use-package image-file
  :if (getenv "DISPLAY")
  :init (auto-image-file-mode 1))


;; Tweaks for terminals
(use-package faces
  :config (add-to-list 'term-file-aliases '("foot" . "xterm")))

;;
;; The easiest solution is to locally install one of the Nerd Fonts
;; from https://www.nerdfonts.com/ where you can get fonts that have
;; been pimped out with the rest of the unicode space. Otherwise we
;; let unicode-fonts try and do its thing.
;;
(if (--filter (s-prefix-p "DejaVuSansM Nerd Font" it)
              (font-family-list))
    (set-face-attribute 'default nil
                        :family "DejaVuSansM Nerd Font"
                        :height 150
                        :weight 'normal
                        :width 'normal)
  (use-package unicode-fonts
    :ensure t
    :if (and (getenv "DISPLAY") (locate-library "unicode-fonts"))
    :config (unicode-fonts-setup))

  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 145
                      :weight 'normal
                      :width 'normal))

(use-package frame
  :config (setq default-frame-alist '((fullscreen . 'fullboth)
                                      (vertical-scroll-bars))
                frame-background-mode 'dark))

(use-package nerd-icons
  :config (setq nerd-icons-font-family "DejaVuSansM Nerd Font"))

;; Prettier unique buffer names.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Mouse set-up
;
; I don't use the mouse for a lot of things and on my netbook it
; positively gets in the way. Really it is only used for links and the
; occasional scroll of the buffer.

;; Stop the mouse cursor getting in the way. This is great.
(use-package avoid
  :if window-system
  :defer 60
  :config
  (mouse-avoidance-mode 'exile))

;; enable the mouse wheel
(use-package mwheel
  :if window-system
  :commands mwheel-install
  :init (mwheel-install))

; X11 paste to point
(when (boundp 'mouse-yank-at-point)
  (setq mouse-yank-at-point t))

;; Change the cursor colour in Ovwrt mode
(defun ins-cursor-set ()
  "Set cursor colour according to insert mode."
  (set-cursor-color
   (if overwrite-mode
       "red"
     "grey")))

(use-package simple
  :config
  (add-hook 'overwrite-mode-hook 'ins-cursor-set))

(if I-am-emacs-21+
    (blink-cursor-mode -1))

(setq frame-title-format "%b")
(setq  icon-title-format "%b")

(defun my-flash-modeline ()
  "Flash the modeline."
  (let ((orig-fg (face-foreground 'mode-line)))
    (set-face-foreground 'mode-line "#F2804F")
    (run-with-idle-timer
     0.1 nil
     (lambda (fg)
       (set-face-foreground 'mode-line fg))
     orig-fg)))

(setq cursor-type 'box
      visible-bell t
      ring-bell-function 'my-flash-modeline)

;; Make fill do the Right Thing with full-stops.
(setq sentence-end-double-space nil)

;; Highlights region _all_ the time. Slightly buggy...
(transient-mark-mode t)
(delete-selection-mode 1)

;; Tweaks to scrolling behaviour. Still a bit odd.
(setq scroll-preserve-screen-position t
      scroll-conservatively 5
      scroll-step 1
      next-line-add-newlines nil)

;; Make pound signs work
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;(setq unibyte-display-via-language-environment t)

;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

(defun my-persist-theme (&optional frame)
  "Persist the current theme"
  (when custom-enabled-themes
    (load-theme (car custom-enabled-themes) t)))

(add-hook 'after-make-frame-functions 'my-persist-theme)

(message "Done Display Hacks")


;; I hate tabs - they are set in cc-mode but not everything respects that
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Whitespace mode
(use-package whitespace
  :commands whitespace-mode
  :config
  (setq whitespace-style '(face
                           tabs trailing lines-tail empty
                           space-after-tab tab-mark)))

;; WIP: what I want is to be able to re-size without being locked to
;; the golden ratio
(defun my-shrink-window-horizontally (delta)
  "Temporarily disable golden-ratio before shrinking window."
  (interactive "p")
  (if (not golden-ratio-mode)
      (shrink-window-horizontally delta)
    (golden-ratio-mode -1)
    (shrink-window-horizontally delta)))

;; (defadvice shrink-window
;;     (before disable-golden-ratio)
;;   "Disable golden ratio before changing window size."
;;   (golden-ratio -1))

;; (defadvice enlarge-window
;;     (before disable-golden-ratio)
;;   "Disable golden ratio before changing window size."
;;   (golden-ratio -1))

(defun my-ediff-comparison-buffer-p ()
  "Safely check if in an ediff session"
  (and (boundp 'ediff-this-buffer-ediff-sessions)
       ediff-this-buffer-ediff-sessions))

;; Nice window sizing
(use-package golden-ratio
  :ensure t
  :commands (golden-ratio-mode)
  :defer 30
  :config (setq golden-ratio-exclude-modes '("mu4e-headers-mode"
                                             "mu4e-view-mode"
                                             "gnus-summary-mode"
                                             "ediff-mode"
                                             "my-diff-mode"
                                             reb-lisp-mode)
                golden-ratio-inhibit-functions
                #'my-ediff-comparison-buffer-p)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  :init (golden-ratio-mode))

;; Manual colour themes
(setq custom-safe-themes
      '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
        "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
        "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))

(use-package facemenu
  :commands list-colors-display)

(use-package moe-theme
  :ensure t
  :commands moe-dark
  :init (when window-system
          (moe-dark)))

(use-package gruvbox-theme
  :ensure t
  :init (when (not window-system)
          (load-theme 'gruvbox t)))

(use-package zenburn-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(provide 'my-display)
;;; my-display.el ends here

