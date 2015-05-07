;;; my-toggles.el --- a bunch of toggle operations
;;
;;; Commentary:
;;
;; Inspired by the http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;;
;;; Code:

(require 'my-org)
(require 'use-package)

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
(define-key my-toggle-map "l" 'visual-line-mode)

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


;; Toggle tabs
(defun my-toggle-tabs ()
  "Toggle tabs"
  (interactive)
  (if indent-tabs-mode
      (setq indent-tabs-mode nil)
    (setq indent-tabs-mode t
          c-basic-offset tab-width)))

(define-key my-toggle-map "\t" 'my-toggle-tabs)


;; God-mode
(use-package god-mode
  :commands god-mode-all
  :requires my-toggles
  :init (define-key my-toggle-map "g" 'god-mode-all)
  :config
  (progn
    (defun my-update-god-cursor ()
      "Update the cursor style depending on status of god-mode."
      (setq cursor-type (if (or god-local-mode buffer-read-only)
                            'hollow
                          'box)))
    (add-hook 'god-mode-disabled-hook 'my-update-god-cursor)
    (add-hook 'god-mode-enabled-hook 'my-update-god-cursor)))

(provide 'my-toggles)
;;; my-toggles.el ends here
