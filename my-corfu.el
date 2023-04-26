;;; my-corfu --- Corfu customisation
;;
;;; Commentary:
;;
;; This is an alternative completion engine to company. Currently I am
;; experimenting with it as an alternative.
;;
;;; Code:


;; Helpers for tweaking completion, we want flex first followed by
;; orderless.

(defun my-orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

;; ensure we use flex matching
(defun my-lsp-mode-setup-completion ()
  "Setup flex matching for LSP."
  (message "setting up flex matching")
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))) ;; Configure flex

;; Optional cape package.
;; See the Cape README for more tweaks!
(use-package cape
  :ensure t)

(use-package orderless
  :ensure t
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil)
  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my-orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster. FIXME
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

(use-package corfu
  :ensure t
  :hook (lsp-completion-mode . my-lsp-mode-setup-completion)
  :init (global-corfu-mode)
  :config (setq corfu-auto t))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :init (corfu-terminal-mode +1)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

  ;; :custom (lsp-completion-provider :none) ;; we use Corfu!
  ;; :init
  ;; (defun my-lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(flex))) ;; Configure flex

  ;; :custom (lsp-completion-provider :none) ;; we use Corfu!
  ;; :init

(provide 'my-corfu)
;;; my-corfu.el ends here
