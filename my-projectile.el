;;; my-projectile.el --- Customisation for projectile
;;
;;; Commentary:
;;
;; Customisation for projectile (https://github.com/bbatsov/projectile)
;;
;;; Code:
;;

(eval-when-compile
  (require 'use-package))

(defun my-projectile-project-find ()
  "Do a find across the projectile project."
  (interactive)
  (my-project-find (projectile-project-root)))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :bind (:map projectile-mode-map
              ("<f5>" . my-projectile-project-find)
              ("C-c c" . projectile-compile-project))
  :config
  (progn
   (setcdr (assq 'projectile-mode minor-mode-alist) '(" prji"))))

(use-package helm-projectile
  :disabled t
  :bind ("C-c h" . helm-projectile))

(provide 'my-projectile)
;;; my-projectile.el ends here
