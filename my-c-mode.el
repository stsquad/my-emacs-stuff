;;; my-c-mode.el ---  My C Mode Customisations
;;
;;; Commentary:
;;
;; I have declared c-mode bankruptcy and removed all customisations
;; and style guessing code from here. This is better handled by things
;; like .dir-locals or editorconfig fles in each project.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)

(use-package cc-mode
  :commands c-mode)

;; enhance C/C++ Development

(defun my-irony-cdb-setup ()
  "Wrapper around `irony-cdb-autosetup-compile-options'.

This is simply to avoid trying to load when dealing with header files
  as they are generally not in compilation databases."
  (unless (s-suffix? ".h" (buffer-file-name))
    (irony-cdb-autosetup-compile-options)))

(defun my-warn-irony-cdb-not-found (command &rest args)
  "Echo a message if we got COMMAND get-compile-options, ARGS is ignored.
We must have reached the end of irony-cdb-compilation-databases."
  (when (eq command 'get-compile-options)
    (message "Irony: compile options not found!")
    nil))

;;
;; The finding of compile commands is a little magic. We have an
;; alist:
;;   irony-cdb-json--project-alist
;;
;; which you can add to with:
;;   irony-cdb-json-add-compile-commands-path
;;
;; which btw is in:
;; ~/.emacs.d/irony/cdb-json-projects


(setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                irony-cdb-libclang
                                                my-warn-irony-cdb-not-found))
(use-package irony
  :disabled t
  :hook ((c-mode . irony-mode)))

(when I-am-at-work
  (use-package irony-eldoc
    :disabled t
    :config (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package lsp-mode
  :hook (c-mode . lsp-mode)
  :ensure t
  :config
  (setq
   lsp-prefer-flymake nil
   lsp-clients-clangd-executable
   (which-lookup '("clangd-7" "clangd"))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
;;
;; End of c-mode customisations
;;

(provide 'my-c-mode)
