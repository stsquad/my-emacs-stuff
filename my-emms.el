;;
;; EMMS Setup
;;

(require 'emms-setup)
(require 'emms-info)
(require 'emms-info-mp3info)
(require 'emms-info-ogginfo)
(require 'emms-browser)

(emms-all)
(emms-default-players)

;;(remove-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")
;(setq emms-track-description-function emms-track-simple-description)

;; When asked for emms-play-directory,
;; always start from this one 
(setq emms-source-file-default-directory (if I-am-at-work
                                             "/export/music"
                                           "/home/alex/Music")
      emms-playlist-buffer-name "*Music*"
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

;; (setq emms-track-description-function 'fc-emms-track-description)
;; (defun fc-emms-track-description (track)
;;   "Return a nice description of TRACK."
;;   (let ((desc (emms-track-simple-description track)))
;;     (if (string-match "^/home/forcer/snd/\\(musik/\\(artists/\\)?\\)?\\(.*\\)" 
;; desc)
;;         (match-string 3 desc)
;;       desc)))

(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(define-key dired-mode-map (kbd "C-q") 'emms-add-dired)


(require 'emms-mode-line)
(emms-mode-line nil)


