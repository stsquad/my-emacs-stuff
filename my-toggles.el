;;; my-toggles.el --- a bunch of toggle operations
;;
;;; Commentary:
;;
;; Inspired by the http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;;
;;; Code:

(require 'my-org)
(require 'my-hydra)
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
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block)
                               t))
               (t
                (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
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

(defun my-undo-status ()
  "Report a string describing the undo status"
  (if (and (not (sequencep buffer-undo-list)))
      (format "disabled")
    (format "%d entries" (length buffer-undo-list))))

(define-key my-toggle-map "u" 'my-toggle-buffer-undo)


;; Toggle tabs
(defun my-toggle-tabs ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (if indent-tabs-mode
      (setq indent-tabs-mode nil)
    (setq indent-tabs-mode t
          c-basic-offset tab-width)))

(define-key my-toggle-map "\t" 'my-toggle-tabs)

;; Toggle M-SPC binding
(defun my-meta-space-status ()
  "Report the current M-SPC binding."
  (key-binding (kbd "M-SPC")))

(defun my-toggle-meta-space ()
    "Cycle through settings for M-SPC. This is mainly for the benefit
of things where C-SPC can't be used."
  (interactive)
  (let ((cmd (my-meta-space-status)))
    (global-set-key (kbd "M-SPC")
                    (cond
                     ((eq cmd 'cycle-space) 'my-mark-or-expand-dwim)
                     (t 'cycle-space)))))

;; Macro to expand the variable and helper function to toggle a major
;; mode.
(defmacro my-make-mode-toggle (mode)
  "Create a `MODE' toggle function/state variable."
  (let* ((mode-name (symbol-name mode))
         (mode-toggle-prefix (format "my-toggle-%s" mode-name))
         (mode-toggle-func (make-symbol mode-toggle-prefix))
         (mode-toggle-var  (make-symbol
                            (format "%s-last-major-mode" mode-toggle-prefix))))
    `(progn
       ;; define the state variable
       (defvar ,mode-toggle-var nil
         ,(format
           "Previous `major-mode' of buffer before toggling %s."
           mode-name))
       (make-local-variable (quote ,mode-toggle-var))
       (put (quote ,mode-toggle-var) (quote permanent-local) t)
       ;; define the toggle function
       (defun ,mode-toggle-func ()
         ,(format "Toggle `%s' in current buffer" mode-name)
         (interactive)
         (if (and (eq major-mode (quote ,mode))
                  ,mode-toggle-var)
             (funcall ,mode-toggle-var)
           (setq ,mode-toggle-var major-mode)
           (,mode))))))

;; Define some mode toggles
(my-make-mode-toggle text-mode)
(my-make-mode-toggle org-mode)

;;
;; Now define some hydra's
;;

(defhydra my-mode-toggle
  (:hint nil :color blue :timeout 5)
  (concat "Toggle mode from %`major-mode to ")
  ("o" my-toggle-org-mode "org-mode")
  ("t" my-toggle-text-mode "text-mode"))

(defhydra my-debug-toggle
  (:hint nil :color blue :timeout 5)
  (concat
   "Debug Toggles: d-o-_e_: %`debug-on-error d-o-_q_: %`debug-on-quit "
   "d-o-_m_: %`debug-on-message _a_ll _r_eset debug e_x_it\n")
  ("e" toggle-debug-on-error :color red)
  ("q" toggle-debug-on-quit :color red)
  ("m" (lambda() ;; Xxx fix to return nil on empty string
         (interactive)
         (setq debug-on-message
               (completing-read "debug-on-message: " nil nil nil nil
  nil nil))))
  ("x" nil :exit t)
  ("a" (lambda ()
         (interactive)
         (setq debug-on-error t
               debug-on-quit t)))
  ("r" (lambda ()
         (interactive)
         (setq debug-on-error nil
               debug-on-quit nil))))

;; Master toggle function
(require 'whitespace)
(global-set-key
 (kbd "C-x t")
 (defhydra my-hydra-toggle (:hint nil :color blue :timeout 5)
   (concat
    "_f_ill:%`auto-fill-function _T_abs: %`indent-tabs-mode "
    "_u_ndo: %s(my-undo-status) meta _s_pace: %s(my-meta-space-status)\n")
   ;; Fill, whitespace and other editing modes
   ("f" auto-fill-mode)
   ("T" my-toggle-tabs)
   ("u" my-toggle-buffer-undo)
   ("s" my-toggle-meta-space)

   ;; Auto-strings
   ("l" visual-line-mode "visual-line")
   ("w" whitespace-mode "whitespace")

   ;; Narrowing, region selection
   ("n" my-narrow-or-widen-dwim "arrow-or-w")
   ("e" er/expand-region "xpand-r")

   ;; Chained Hydras
   ("d" my-debug-toggle/body "ebug toggles")
   ("m" my-mode-toggle/body "mode toggles")

   ;; quit the hydra
   ("x" nil "exit" :exit t)))

(provide 'my-toggles)
;;; my-toggles.el ends here
