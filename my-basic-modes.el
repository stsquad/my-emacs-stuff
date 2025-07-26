;;; my-basic-modes --- Common global modes
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
;; This enables all the common built-in modes I use in Emacs.
;; Specifically these are modes enabled at start and not on demand.
;;
;;; Code:

(require 'use-package)
(require 'my-libs)

;; Automagically decompress files
(auto-compression-mode t)

;; Save history
(use-package savehist
  :init (savehist-mode 1)
  :config (apply 'add-to-list
                 '(savehist-additional-variables compile-history)))

;; Don't prompt me to revert something
(use-package autorevert
  :defer 240
  :init (global-auto-revert-mode 1))

;; Simple access to the calculator
(use-package calculator
  :bind ("C-<return>" . calculator))

(use-package calc-mode
  :bind (:map calc-mode-map
              ("C-x t" . casual-calc-tmenu)))

;; Recentf
;;
;; This is mainly for the benefit of helm-mini
(use-package recentf
  :commands recentf-mode
  :config (setq recentf-max-saved-items nil
                recentf-exclude '("Maildir/.*"))
  :init (recentf-mode))

;; Help+
(use-package help-fns+
  :commands describe-keymap)

;;
;; Narrowing Frameworks
;;
;; I include Counsel/Ivy/Swiper packages as others support it, but
;; experimenting with vertico and embark
;;
(use-package ivy
  :ensure t
  ;; :load-path (lambda () (my-return-path-if-ok
  ;;                        "~/src/emacs/swiper.git"))
  :commands ivy-mode
  ;; :init (ivy-mode)
  :config
  (setq
    ivy-use-virtual-buffers t
    ivy-count-format "%d/%d "
    ivy-re-builders-alist
    '((ivy-switch-buffer . ivy--regex-plus)
      (t . ivy--regex-plus))))

;; Casual provides a whole bunch of transient menus for various modes
;; however it does require at least Emacs 29.1 (bookworm backports
;; will do).
(use-package casual
  :ensure t
  :bind (("C-c c" . casual-compile)
         (:map bookmark-bmenu-mode-map
               ("C-x t" . casual-bookmarks-tmenu))
         (:map dired-mode-map
               ("C-x t" . casual-dired-tmenu))
         (:map eshell-mode-map
               ("C-x t" . casual-eshell-tmenu))))


;; Counsel
;;
;; I mostly use counsel-compile (although the descbinds and apropos
;; helpers are also nice). Narrowing is now handled mostly by vertico.
;;
(use-package counsel
  :ensure t
  ;; :load-path (lambda () (my-return-path-if-ok
  ;;                        "~/src/emacs/swiper.git"))
  ;; individually bind the useful functions
  :bind (("C-h b" . counsel-descbinds)
         ("C-h a" . counsel-apropos)
         ("C-c c" . counsel-compile)
         (:map counsel-compile-map
               ("C-j" . ivy-immediate-done))))

;; Enable vertico
(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("<prior>" . vertico-scroll-down)
         ("<next>" . vertico-scroll-up))
  :config (setq vertico-count 20
                vertico-resize nil
                vertico-multiform-commands
                '((my-lusty-file-explorer unobtrusive))
                vertico-multiform-categories
                '((consult-grep buffer)))
  :init (vertico-mode 1) (vertico-multiform-mode 1))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g"  . consult-goto-line)           ;; orig. goto-line
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config (setq consult-preview-key "M-.")
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key "M-."
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
)


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-annotator-registry '(file none marginalia-annotate-file))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Also icons ;-)
(when have-melpa
  (use-package nerd-icons-completion
    :ensure t
    :config
    (nerd-icons-completion-mode)))

(use-package embark
  :ensure t
  :bind
  (("C-c C-." . embark-act)         ;; pick some comfortable binding
   ("C-c C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'my-basic-modes)
;;; my-basic-modes.el ends here
