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

; fixes for compile
(eval-when-compile
  (declare-function crmbk-running-in-host-x11-p "chromebook")
  (defvar crmbk-frame-mode-map))

(when (and (require 'chromebook "chromebook" t)
           (crmbk-running-in-host-x11-p))
  (set-face-attribute 'default nil :height 250)
  (add-hook 'crmbk-frame-mode-hook 'crmbk-remap-search)
  (add-hook 'crmbk-frame-mode-hook 'crmbk-disable-touchpad)
  (define-key crmbk-frame-mode-map (kbd "<M-up>") 'scroll-down)
  (define-key crmbk-frame-mode-map (kbd "<M-down>") 'scroll-up)
  (when (boundp 'edit-server-new-frame-alist)
    (setq edit-server-new-frame-alist '((name . "Edit Server Frame")
                                        (fullscreen . 'fullboth)))))

; Re-use existing frames if buffer already exists in one
(unless I-am-on-pixel
  (setq-default display-buffer-reuse-frames t)

  ; messing about - what about dynamic-font stuff?
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 145
                      ;; :family "Symbola"
                      ;; :height 180
                      :weight 'normal
                      :width 'normal))
;;   (when (functionp 'set-fontset-font)
;;     (set-fontset-font "fontset-default" nil
;;                       (font-spec :size 20
;;                                  :name "Symbola"))))
;;     (set-fontset-font "fontset-default"
;;                       'unicode
;;                       (font-spec :family "DejaVu Sans Mono"
;;                                  :width 'normal
;;                                  :size 12.4
;;                                  :weight 'normal))))

(ignore-errors
  (when (require 'zenburn-theme)
    (load-theme 'zenburn t)
    (when (custom-theme-enabled-p 'zenburn)
      (zenburn-with-color-variables
        (custom-theme-set-faces
         'zenburn
         `(num3-face-odd ((t (:foreground ,zenburn-fg-1))))
         `(num3-face-even ((t (:foreground ,zenburn-fg+1)))))))))

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

;; want to reduce the amount of white space in the mode-line
(setq global-mode-string
      '("" org-mode-line-string))

(setq-default mode-line-format
              '("-"
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                " "
                "%l/%c "
                "%[("
                mode-name
                mode-line-process
                minor-mode-alist
                "%n"
                ")%]"
                "--"
                global-mode-string
                "--"
                ))



;; Let's shrink the minor-mode-alist down to size.
(setcdr (assq 'abbrev-mode minor-mode-alist) '(" Ab"))
(setcdr (assq 'auto-fill-function minor-mode-alist) '(" Fl"))

;; Not added until the relevant mode is loaded.
(setq minor-mode-alist (cons '(compilation-in-progress nil)
                             minor-mode-alist))

;; Uses a separate variable. Isn't that nice?
(setq eldoc-minor-mode-string nil)

;; (display-time) is needed for appt to display in the mode-line, but
;; we don't want the time taking up precious space.
(require 'time)
(setq display-time-interval 20
      display-time-format 'nil
      display-time-string-forms '( 24-hours ":" minutes ))
(display-time-mode)

;; Displays current function() in programming modes.
(when (require 'which-func nil t)
  (which-function-mode))

;; Reduce white space
(setq-default mode-line-buffer-identification '("%b"))

(when (require 'smart-mode-line nil t)
  (sml/setup))

(eval-when-compile (defvar tracking-most-recent-first))
(when (require 'tracking nil t)
  (setq tracking-most-recent-first t)
  (tracking-mode))

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

;; Don't prompt me to revert something
(global-auto-revert-mode 1)

;; I hate tabs - they are set in cc-mode but not everything respects that
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

; TODO: clean-up my defaults for this
(defvar whitespace-style)
(when (require 'whitespace nil t)
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

