;;; my-windows --- Window moving code
;;
;;; Commentary:
;;
;; This contains all the window navigation customisation I use in one
;; handy place.
;;
;;; Code:

;; Require prerequisites
(eval-when-compile (require 'use-package))
(use-package my-vars)

(defun my-current-frame-is-portrait-p ()
  "Return t if the current frame is in portrait mode."
  (< (/ (float (frame-width)) (frame-height)) 1.5))

(defun my-display-new-buffer-in-narrow-frame (buffer alist)
  "Returns t if its worth creating a new window."
  (and (my-current-frame-is-portrait-p)
       (buffer-file-name buffer)
       (> 2 (length (window-list)))))

;;
;; See: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;;
(use-package window
  :config (setq
           display-buffer-reuse-frames t  ;; Re-use existing frames if buffer already exists in one
           display-buffer-alist
           '((my-display-new-buffer-in-narrow-frame
              (display-buffer-in-direction) (direction . above))
             ("*Article*"
              (display-buffer-in-side-window)
              (side . top) (slot . 1)))
           display-buffer-base-action nil))

;; Window navigation and size control
(use-package windmove
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings))

;; select a winner
(defun my-winner-ivy-select ()
  "Select a winner configuration and switch to it."
  (interactive)
  (let (collection)
    (setq collection
          (--map
           (cons (format "%d:%s" (length it)
                         (-map 'cdr (cdr it)))
                 it)
           (ring-elements winner-pending-undo-ring)))
    (ivy-read "winner:"
              collection
              :action (lambda (conf)
                        (winner-set (cdr conf))))))

(global-set-key
 (kbd "C-x w")
 (defhydra my-hydra-windows (:hint nil :color red :timeout 5)
   (concat
    "_p_revious: of %s(ring-length winner-pending-undo-ring) "
    "_n_ext _f_ind <RET> when done\n")
   ("p" winner-undo nil)
   ("n" winner-redo nil)
   ("f" my-winner-ivy-select nil :exit t)
   ("<RET>" nil :exit t)))

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
  :ensure t
  :bind ("C-x o" . ace-window)
  :config (setq
           aw-scope 'visible ;; jump between visible frames across displays
           ))

(provide 'my-windows)
;;; my-windows.el ends here
