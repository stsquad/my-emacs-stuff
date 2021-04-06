;;; my-elfeed --- Elfeed configuration
;;
;;; Commentary:
;;
;; Elfeed provides a useful way to ingest RSS/Atom feeds. It doesn't
;; replace Newsblur but it's handy for tracking things.
;;
;;; Code:

(use-package elfeed
  :ensure t
  :config (setq elfeed-log-level 'debug
                elfeed-use-curl 't))

(use-package elfeed-org
  :config (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

(provide 'my-elfeed)
;;; .el ends here
