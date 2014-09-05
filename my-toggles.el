;;; my-toggles.el --- a bunch of toggle operations
;;
;;; Commentary:
;;
;; Inspired by the http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;;
;;; Code:
(require 'org)

(defvar my-toggle-map
  nil
  "A set of toggle maps.")

(define-prefix-command 'my-toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'my-toggle-map)

(define-key my-toggle-map "d" 'toggle-debug-on-error)
(define-key my-toggle-map "q" 'toggle-debug-on-quit)
(define-key my-toggle-map "f" 'auto-fill-mode)
(define-key my-toggle-map "w" 'whitespace-mode)

;; Narrowing
; from: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun my-narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens.  Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(define-key my-toggle-map "n" 'my-narrow-or-widen-dwim)

;; Undo control
; turn undo on or off for the buffer

(defun my-toggle-buffer-undo ()
  "Toggle undo tracking in current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (if (eq buffer-undo-list t)
        (setq buffer-undo-list nil)
      (buffer-disable-undo (current-buffer)))))

(define-key my-toggle-map "u" 'my-toggle-buffer-undo)

(provide 'my-toggles)
;;; my-toggles.el ends here
