;;; my-corfu --- Corfu customisation -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is an alternative completion engine to company. Currently I am
;; experimenting with it as an alternative.
;;
;;; Code:

(eval-when-compile (require 'use-package))

;; Helpers for tweaking completion, we want flex first followed by
;; orderless.

(defun my-orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

;; Optional cape package.
;; See the Cape README for more tweaks!
(use-package cape
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

  ;; :init
  ;; ;; Optionally configure the first word as flex filtered.
  ;; (add-hook 'orderless-style-dispatchers #'my-orderless-dispatch-flex-first nil 'local))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :config (setq corfu-auto t))

;; we also need to handle disabling on new frames
(defun my-maybe-enable-corfu-terminal-mode ()
  "Enable corfu-terminal-mode if non-graphic frame.

This should be called from a hook such as `server-after-make-frame-hook'"
  (if (display-graphic-p)
      (corfu-terminal-mode -1)
    (corfu-terminal-mode +1)))

(use-package corfu-terminal
  :ensure t
  :hook (server-after-make-frame . my-maybe-enable-corfu-terminal-mode))

;; Disable for now as it breaks ement:
;;  https://github.com/alphapapa/ement.el/issues/212
;;  https://github.com/rougier/svg-lib/issues/18
(use-package kind-icon
   :disabled t
   :after corfu
   :custom
   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
   :config
   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; a few more useful configurations...
(use-package emacs
  :init (setq
         completion-cycle-threshold 3 ;; TAB cycle if there are only few candidates
         tab-always-indent 'complete)) ;; Enable indentation+completion using the TAB key

(provide 'my-corfu)
;;; my-corfu.el ends here
