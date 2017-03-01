;;; my-emms --- EMMS Setup
;;
;;; Commentary:
;;
;; 
;;
;;; Code:

;; Require prerequisites
(eval-when-compile (require 'use-package))
(require 'my-vars)

(use-package emms-setup
  :config
  (emms-all)
  (emms-default-players))

(use-package emms
  :map
  :config
  (setq emms-source-file-default-directory (if I-am-at-work
                                               "/export/music"
                                             "/home/alex/media/music")
        emms-playlist-buffer-name "*Music*"
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
        emms-show-format "NP: %s"
        emms-track-description-function 'emms-track-simple-description))


(use-package emms-info-mp3info)
(use-package emms-info-ogginfo)
(use-package emms-browser)

(provide 'my-emms)
;;; my-emms.el ends here
;;       emms-playlist-buffer-name "*Music*"
;;       emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

;; (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
;; (define-key dired-mode-map (kbd "C-q") 'emms-add-dired)
