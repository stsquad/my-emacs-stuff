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
(require 'use-package)

;; Window navigation and size control
(use-package windmove
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings))

;; winner-mode to remember window layouts
(use-package winner
  :commands winner-mode
  :init (winner-mode t))

;; Window key bindings
(use-package window
  :if (not (and (fboundp 'crmbk-running-in-host-x11-p)
                (crmbk-running-in-host-x11-p)))
  :bind (("M-<down>" . enlarge-window)
         ("<M-right>" . enlarge-window-horizontally)
         ("<M-up>" . shrink-window)
         ("<M-left>" . shrink-window-horizontally)))

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


;; ace-window makes switching less painful
(use-package ace-window
  :ensure ace-window
  :bind ("C-x o" . ace-window)
  :config (setq aw-scope 'frame))

(provide 'my-windows)
;;; my-windows.el ends here
