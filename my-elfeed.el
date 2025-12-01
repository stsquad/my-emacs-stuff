;;; my-elfeed --- Elfeed configuration
;;
;;; Commentary:
;;
;; Elfeed provides a useful way to ingest RSS/Atom feeds. It doesn't
;; replace Newsblur but it's handy for tracking things.
;;
;;; Code:

(eval-when-compile (require 'use-package))
(use-package my-package)

(use-package elfeed
  :ensure t
  :config (setq elfeed-log-level 'debug
                elfeed-use-curl 't
                elfeed-curl-extra-arguments '("-H" "Cache-Control: no-cache")))

(when have-melpa
  (use-package elfeed-org
    :ensure t
    :config (progn
              (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))
              (elfeed-org))))

(provide 'my-elfeed)
;;; .el ends here
