;;; my-keybinds --- Global keybindings
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
;; All my global keybinds in here that aren't mode specific.
;;
;; I'm allowed to bind: C-c [letter], [f5]-[f9]
;;
;; NB I've also re-mapped Caps-Lock to control
;;
;;; Code:

;; Function Keys, I'm currently going over what my Microsoft ergonomic
;; keyboard has sentsiled on the function keys

; Find help
(global-set-key (kbd "<C-f1>") 'apropos)

; Make Undo a little less octopedal
(global-set-key (kbd "<f2>") 'undo)

;; In Emacs 21+, home and end go to beginning and end of line. This is
;; clearly the Wrong Thing.
(when I-am-emacs-21+
  (global-unset-key [home])
  (global-set-key [home] 'beginning-of-buffer)
  (global-unset-key [end])
  (global-set-key [end] 'end-of-buffer))

;; Macro keys
; If I define a single press macro keys I may use them more often

(global-set-key [(control meta m)] 'call-last-kbd-macro)
(global-set-key (kbd "<f10>") 'call-last-kbd-macro)
(global-set-key (kbd "<C-f11>") 'start-kbd-macro)
(global-set-key (kbd "<C-f12>") 'end-kbd-macro)

; This stops warnings with re-mapped
; return (when using xcape)
(global-set-key (kbd "<key-4660>") 'ignore)

;; This gets over-ridden when company/auto-complete is in effect
(global-set-key (kbd "M-/") 'hippie-expand)

;; Goto-line should be easy
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\M-g" 'goto-line)

;; Return => newline-and-indent
(global-unset-key "\C-m")
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Make delete do what I expect
;? Do I still need this?
(global-unset-key [delete])
(global-set-key [delete] 'delete-char)

;; C-z is too easy to hit (you can still C-x C-z to suspend)
(global-unset-key (kbd "C-z"))

;; C-k deletes whole line
(global-set-key "\C-k" 'kill-whole-line)

;; Let's also do Alt-X and easier way
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "<C-tab>") 'pop-global-mark)

;; Handle next/prev error on keymap / and * (with numlock off)
(global-set-key (kbd "M-O o") 'previous-error)
(global-set-key [kp-divide] 'previous-error)
(global-set-key (kbd "M-O j") 'next-error)
(global-set-key [kp-multiply] 'next-error)

(provide 'my-keybinds)
;;; my-keybinds.el ends here

