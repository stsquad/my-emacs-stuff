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

;;; Display hacks
;
; With --daemon mode using start-up evaluation is no longer useful as
; the .emacs will almost certainly run without window-system meaning
; anything. Instead we should do things on the fly as new frames are
; created.


;; Disable the menu and tool bars, they just take up space.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use imagemagick if we have it to view images
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

; default-frame-alist
(setq default-frame-alist '((fullscreen . 'fullboth)
                            (vertical-scroll-bars)))

; Re-use existing frames if buffer already exists in one
  (setq-default display-buffer-reuse-frames t)

  ; messing about - what about dynamic-font stuff?
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 145
                      ;; :family "Symbola"
                      ;; :height 180
                      :weight 'normal
                      :width 'normal)

(message "Display Done")

;; Prettier unique buffer names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Mouse set-up
;
; I don't use the mouse for a lot of things and on my netbook it
; positively gets in the way. Really it's only used for links and
; the occasional scroll of the buffer.

;; Stop the mouse cursor getting in the way. This is great.
(unless I-am-xemacs
  (when 'window-system
    (mouse-avoidance-mode 'exile)))

;; enable the mouse wheel
(autoload 'mwheel-install "mwheel" "Enable wheely mouse")
(mwheel-install)

; X11 paste to point
(when (boundp 'mouse-yank-at-point)
  (setq mouse-yank-at-point t))

;; Change the cursor colour in Ovwrt mode
(defun ins-cursor-set ()
  "Set cursor colour according to insert mode."
  (set-cursor-color
   (if overwrite-mode
       "red"
     "black")))

(if I-am-emacs-21+
    (blink-cursor-mode -1))

(add-hook 'post-command-hook 'ins-cursor-set)

(setq frame-title-format "%b")
(setq  icon-title-format "%b")


;; Make fill do the Right Thing with full-stops.
(setq sentence-end-double-space nil)

;; Highlights region _all_ the time. Slightly buggy...
(transient-mark-mode t)
(delete-selection-mode 1)

;; Groovy things with matching parentheses
(show-paren-mode 1)

;; Tweaks to scrolling behaviour. Still a bit odd.
(setq scroll-preserve-screen-position t
      scroll-conservatively 5
      scroll-step 1
      next-line-add-newlines nil)

;; Make pound signs work
(set-language-environment "UTF-8")
(setq unibyte-display-via-language-environment t)

;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

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

;; Bow down before font-lock
(add-hook 'font-lock-mode-hook
          '(lambda ()
             (setq font-lock-maximum-decoration  t
                   font-lock-verbose             t
                   font-lock-support-mode        'jit-lock-mode
                   lazy-lock-defer-on-scrolling  nil
                   lazy-lock-defer-contextually  t
                   lazy-lock-stealth-verbose     t
                   lazy-lock-stealth-lines       50
                   lazy-lock-stealth-time        3)))
(global-font-lock-mode t)


;; Font locking info mode (from Andy.Ling@quantel.com)
(defvar info-font-lock-keywords
  (list
   '("^\\* [^:]+:+" . font-lock-function-name-face)
   '("\\*[Nn]ote\\b[^:]+:+" . font-lock-reference-face)
   '("  \\(Next\\|Prev\\|Up\\):" . font-lock-reference-face))
  "Additional expressions to highlight in Info mode.")

(add-hook 'Info-mode-hook
          (lambda ()
            (make-local-variable 'font-lock-defaults)
            (setq
             font-lock-defaults '(info-font-lock-keywords nil t)
             case-fold-search nil)))


(provide 'my-display)
;;; my-display.el ends here

