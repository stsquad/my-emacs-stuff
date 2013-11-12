;;
;; Tramp Customisations/Tweaks
;;

(require 'tramp)

; You can pretty much guarantee tramp implies over ssh
(setq tramp-default-method "scp")
; Auto-saves tramp files on our local file system
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp "~/.emacs.d/tramp-saves"))
; If I'm travelling assume no one is messing with files on my work
; machine
(when (string-match "symbiot" (system-name))
  (setq remote-file-name-inhibit-cache 'nil
        tramp-completion-reread-directory-timeout 'nil))
; I tend to use magit anyway, but disable VC mode over TRAMP
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

