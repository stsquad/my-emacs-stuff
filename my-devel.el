;;; my-devel.el --- Central configuration for development hooks
;;
;;; Commentary:
;;
;; All other development modes should be triggered from here.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)
(require 'my-find)
(require 'my-tracking)
(require 'my-hydra)

;; EditorConfig
(use-package editorconfig
  :ensure t
  :diminish "EdCf"
  :config
  ;; See https://github.com/editorconfig/editorconfig-emacs/issues/246
  (add-to-list 'editorconfig-exclude-modes 'git-rebase-mode)
  (editorconfig-mode 1))

(use-package editorconfig-custom-majormode
  :ensure t
  :config (add-hook 'editorconfig-custom-hooks 'editorconfig-custom-majormode))

;; Origami code folding
(use-package origami
  :if (locate-library "origami")
  :commands origami-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'origami-mode)
    (with-eval-after-load 'hydra
      (define-key origami-mode-map (kbd "C-c f")
        (defhydra hydra-folding (:color red :hint nil)
   "
_o_pen node    _n_ext fold       toggle _f_orward    _F_ill column: %`fill-column
_c_lose node   _p_revious fold   toggle _a_ll        e_x_it
"
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("f" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("F" fill-column)
   ("x" nil :color blue))))))

(use-package prog-mode
  :hook (prog-mode . turn-on-auto-fill))

;; Regex's

(use-package rx
  :ensure t
  :commands rx)

;; xr allows you to covert regex into rx
(use-package xr)

(defhydra my-reb-hydra (:color teal)
  (concat "<TAB> Toggle syntax: %`reb-re-syntax ")
  ("TAB" reb-change-syntax nil)
  ("t" my-hydra-toggle/body "main toggles")
  ("q" quit-window "quit"))

(use-package re-builder
  :ensure t
  :bind (:map reb-mode-map
              ("C-x t" . my-reb-hydra/body)
         :map reb-lisp-mode-map
              ("C-x t" . my-reb-hydra/body))
  :commands re-builder
  :config (setq reb-re-syntax 'rx))

;;
;; Rust
;;

(use-package rustic
  :ensure t
  :config (progn (add-to-list 'compilation-error-regexp-alist-alist
                              (cons 'rustic-error rustic-compilation-error))
                 (add-to-list 'compilation-error-regexp-alist-alist
                              (cons 'rustic-warning rustic-compilation-warning))
                 (add-to-list 'compilation-error-regexp-alist-alist
                              (cons 'rustic-info rustic-compilation-info))
                 (add-to-list 'compilation-error-regexp-alist-alist
                              (cons 'rustic-panic rustic-compilation-panic))

                 (add-to-list 'compilation-error-regexp-alist 'rustic-error)
                 (add-to-list 'compilation-error-regexp-alist 'rustic-warning)
                 (add-to-list 'compilation-error-regexp-alist 'rustic-info)
                 (add-to-list 'compilation-error-regexp-alist 'rustic-panic)))


;;
;; Compile Mode
;;

(use-package my-c-mode)

;; See: http://emacs.stackexchange.com/questions/3802/how-can-i-detect-compilation-mode-is-waiting-for-input/3807?noredirect=1#comment5796_3807
(defun my-compilation-mode-warn-about-prompt ()
  "Pop up a warning if we stall due to interactive config questions."
  (save-excursion
    (let ((re (rx "[" (one-or-more (any "n" "N" "m" "M" "Y" "y") "/") "?]"
                   (optional " (NEW)") (zero-or-more whitespace) buffer-end)))
      (when (re-search-backward re nil 'no-error)
        (lwarn 'emacs :warning "Compilation process in %s seems stalled!"
               (buffer-name))))))

(defun my-hide-compilation-buffer (proc)
  "Hide the compile buffer `PROC' is ignored."
  (let* ((window (get-buffer-window "*compilation*"))
         (frame (window-frame window))
         (buf (current-buffer)))
    (ignore-errors
      (delete-window window))
    ;; preserve the buffer we are in
    (set-buffer buf)))

(defun my-report-compilation-finished (buf exit-string)
  "Report the compilation buffer `BUF' to tracker."
  (tracking-add-buffer buf))

;; Smart compile commands

(defvar my-core-count
  (string-to-number
   (shell-command-to-string
    "cat /proc/cpuinfo | grep processor | wc -l"))
  "Count of the CPU cores on this system.")

;; compilation-mode error regexs

(defvar my-tsan-compilation-mode-regex
  (rx
   (: bol (zero-or-more blank)      ; start-of-line
      "#" (one-or-more digit)       ; stack depth
      blank
      (one-or-more (in alnum "_"))  ; function name
      blank
      (group-n 1 (one-or-more (in alnum "/.-"))) ; path
      ":"
      (group-n 2 (one-or-more digit))            ; line number
      ))
  "A regex to match lines of the form:

    #1 page_flush_tb_1 /home/alex/lsrc/qemu/qemu.git/translate-all.c:818 (qemu-arm+0x00006000cb42)
")


(use-package compile
  :bind (("C-c c" . counsel-compile)
         ("C-c r" . recompile)
         (:map compilation-mode-map
               ("n" . compilation-next-error)
               ("p" . compilation-previous-error)))
  :diminish ((compilation-in-progress . "*COM*"))
  :config
  (progn
    (setq
     compilation-auto-jump-to-first-error nil
     compilation-scroll-output t
     compilation-window-height 10
     counsel-compile-make-args (format "-j%d" (+ 1 my-core-count)))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list 'tsan my-tsan-compilation-mode-regex 1 2 nil 0))
    (add-to-list 'savehist-additional-variables 'compile-history)
    (add-to-list 'savehist-additional-variables 'counsel-compile-history)
    ;; lets not overtax the regex match on our huge compilation buffers
    (when I-am-at-work
      (setq compilation-error-regexp-alist '(gcc-include gnu tsan
                                                         rustic-error rustic-warning rustic-info rustic-panic)))
    ;; Detect stalls
    (add-hook 'compilation-filter-hook
              #'my-compilation-mode-warn-about-prompt)
    ;; Add tracking to the compilation buffer
    (with-eval-after-load 'tracking
      (add-hook 'compilation-start-hook 'my-hide-compilation-buffer)
      (add-hook 'compilation-finish-functions 'my-report-compilation-finished))))

;; Tags
;;
;; Favour the common xref interface on newer Emacsen
;;

(if (version<= "25.1" emacs-version)
    (use-package gxref
      :ensure t
      :config (add-to-list 'xref-backend-functions
                           'gxref-xref-backend))
  (use-package counsel-gtags
    :ensure t
    :commands counsel-gtags-mode
    :config
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)))


;; checkpatch
;; currently disabled, there is also checkpatch for flycheck
(use-package checkpatch-mode
  :after magit
  :load-path (lambda () (my-return-path-if-ok
                         "~/mysrc/checkpatch-mode.git"))
  :bind (:map magit-commit-section-map
              ("C-x c" . checkpatch-run-from-magit)))

;; shell modes

(use-package sh-mode
  :mode ((".*\.sh$" . sh-mode)
         (".*\.bbclass$" . sh-mode)
         (".*\.bb$" . sh-mode)))

(use-package fish-mode
  :ensure t)

(use-package fish-completion
  :ensure t)

;; asm-mode
;;                                       ;
;; We define some additional regexs to match ARM and TCG style assembler
;
;  ldr q1, [x20, x0]
;  eor v0.16b, v0.16b, v1.16b
;  add_i64 tmp2,tmp2,tmp3
;  movi_i64 tmp7,$0x8
;

(defvar arm-asm-register
  (rx (: (in "," space) ; leading whitespace or separator
         (group (in "qwvx") digit (zero-or-one digit) ;;
                (opt    ; vector type
                 "."
                 digit (zero-or-one digit)
                 (in "bwhsdq"))
                )))
  "Match against an ARM register.")

;; (font-lock-add-keywords 'asm-mode
;;                         '((arm-asm-register (1 'font-lock-variable-name-face))))


(use-package asm-mode
  :if (not (featurep 'gas-mode))
  :config (setq asm-comment-char ?\;))

;; YAML
(use-package yaml-mode
  :ensure t)

;; Handle Makefile.blah
(use-package make-mode
  :mode ((".*\.mak" . makefile-gmake-mode)
         ("Makefile\..*" . makefile-gmake-mode)))

(use-package meson-mode
  :ensure t)

;; Handle expect files
(use-package tcl
  :mode ("\\.expect\\'" . tcl-mode))

;; Markdown
;; Markdown sites
(use-package markdown-mode
  :ensure t
  :config
  (progn
    (setq markdown-reference-location 'end)))


;;
(use-package realgud
  :commands (realgud:gdb realgud:gdb-pid realgud:ipdb))

;; Pairs and parenthesis
;;
;; I used to use smart-parens but it was too much. electric-pair-mode
;; and show-paren-mode seem to be enough for now.
;;

(use-package elec-pair
  :ensure t
  :config (electric-pair-mode))

(use-package paren
  :ensure t
  :config (show-paren-mode))

;; Docker is useful

(use-package dockerfile-mode
  :mode ("\\.docker\\'" . dockerfile-mode)
  :ensure t)

(provide 'my-devel)
;;; my-devel.el ends here
