;;
;; Buffer handling tweaks
;;

;; Buffer Selection
;
; Use lusty-explorer if I can, otherwise leave it to ido-mode which
; has been in emacs since version 22.
;
; Still have a bs-show "all" bound to C-x C-b for when I want to see
; everything

(require 'midnight)
(require 'ibuf-ext)

(message "Setting up buffer handling")

(setq midnight-mode 't)

;; ido-mode - better buffer selection
(ido-mode t)

(when (require 'ido-ubiquitous nil 'noerror)
  (ido-ubiquitous-mode))

;; but if we have lusty still use that...
(when (require 'lusty-explorer nil 'noerror)
  ;; overrride the normal file-opening, buffer switching
  (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
  (global-set-key (kbd "C-x b")   'lusty-buffer-explorer))

;; ibuffer has been around for some time
(defun my-ibuffer-bs-show ()
  "Emulate `bs-show' from the bs.el package."
  (interactive)
  (ibuffer nil "*Ibuffer-my-bs*" '((filename . ".*")) nil t)
  (define-key (current-local-map) "a" 'ibuffer-bs-toggle-all))

(global-set-key (kbd "C-x C-b") 'my-ibuffer-bs-show)

(setq ibuffer-saved-filters
      (quote (("csrc" ((filename . "/export/csrc/*")))
              ("tramp" ((filename . "\\/ssh:")))
              ("irc" ((mode . erc-mode)))
              ("magit" ((mode . magit-status-mode)))
              ("programming" ((or (mode . emacs-lisp-mode)
                                  (mode . cperl-mode)
                                  (mode . c-mode)
                                  (mode . java-mode)
                                  (mode . idl-mode)
                                  (mode . lisp-mode)))))))

(message "Done Buffer Handling Tweaks")

(provide 'my-buffer)
