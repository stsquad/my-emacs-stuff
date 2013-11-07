;;
;; Customisation for projectile (https://github.com/bbatsov/projectile)
;;

(require 'projectile)

;; Enable projectile for everything
(projectile-global-mode)

;; Shrink mode line for mode display
(setcdr (assq 'projectile-mode minor-mode-alist) '(" prji"))

;; Key hooks
; Hook in projectile-compile to normal keybinding
(define-key projectile-mode-map (kbd "C-c c") 'projectile-compile-project)
(define-key projectile-mode-map (kbd "<f5>") 'projectile-ack)

(when (require 'helm-projectile nil 'noerror)
  (global-set-key (kbd "C-c h") 'helm-projectile))



;; Done
(message "Done setting up projectile")



