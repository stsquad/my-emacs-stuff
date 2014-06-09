;;; my-windows --- Window moving code
;;
;;; Commentary:
;;
;; This contains all the window navigation customisation I use in one
;; handy place.
;;
;;; Code:

;; Require prerequisites
(require 'my-vars)

;; Variables

;; Window navigation and size control
(when (maybe-load-library "windmove")
  (windmove-default-keybindings))

(unless (and (fboundp 'crmbk-running-in-host-x11-p)
             (crmbk-running-in-host-x11-p))
  (global-set-key (kbd "<M-down>") 'enlarge-window)
  (global-set-key (kbd "<M-right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "<M-up>") 'shrink-window)
  (global-set-key (kbd "<M-left>") 'shrink-window-horizontally))

;; Allow windows to be dedicated to one thing interactively
;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x %") 'toggle-frame-split)


;; ace-window makes switching less painful, but only if there are more
;; than two windows visible.
(when (require 'ace-window nil t)
  (defun my-other-ace-window ()
    "Switch to other window, using ace-window if there are more than
  2"
    (interactive)
    (unless (active-minibuffer-window)
      (if (> (count-windows) 2)
          (ace-window nil)
        (other-window 1))))

  (global-set-key (kbd "C-x o") 'my-other-ace-window))

(provide 'my-windows)
;;; my-windows.el ends here

