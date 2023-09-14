;;; my-transmission --- Tweaks for Transmission
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
;; A couple of simple customisations to feed stuff into transmission.
;; Either I select torrent files from dired or I can enque an
;; enclosure directly from elfeed.
;;
;;; Code:

; Require prerequisites
(require 'use-package)
(require 's)

;; Variables


;; Code

(defun my-narrow-to-pure-magnet ()
  "From point remove any extraneous information from the magnet link.
Returns the line as a string."
  (interactive)
  (when (thing-at-point 'url)
    (save-excursion
      (move-beginning-of-line nil)
      (when (re-search-forward "&tr" nil t)
        (forward-line 1)
        (delete-region (match-beginning 0) (point)))
      (move-beginning-of-line nil)
      (substring-no-properties (thing-at-point 'line)))))

(defun my-snarf-magnet ()
  "Snarf a link into transmission."
  (interactive)
  (when (thing-at-point 'url)
    (transmission-add (my-narrow-to-pure-magnet))))

(defun my-enable-torrent-snarfing ()
  "Bind torrent snarfing to action key."
  (interactive)
  (local-set-key (kbd "C-c C-c") 'my-snarf-magnet))

(defun my-dired-add-to-transmission ()
  "Add all marked files to transmission."
  (interactive)
  (-map 'transmission-add (dired-get-marked-files)))


(defun my-snarf-first-elfeed-enclosure-as-link ()
  "Find the first enclosure (if it is a torrent) and return the link."
  (let ((enclosures (elfeed-entry-enclosures elfeed-show-entry)))
    (when (and enclosures
               (string-match-p
                "application/x-bittorrent"
                (nth 1 (-first-item enclosures))))
      (-first-item (-first-item enclosures)))))

(defun my-add-first-elfeed-enclosure-to-transmission ()
  "Queue the first enclosure (if it is a torrent)."
  (interactive)
  (let ((torrent (or (my-snarf-first-elfeed-enclosure-as-link)
                     (elfeed-entry-link elfeed-show-entry))))
    (when torrent
      (transmission-add torrent))))

(defvar my-transmission-current-status
  ""
  "Current status of transmission")

(defun my-transmission-update-stats ()
  "Add transmission stats to modeline"
  (transmission-request-async
   (lambda (response)
     (let-alist response
       (setq my-transmission-current-status
             (format
              (concat "%d kB/s down, %d kB/s up; %d/%d torrents active; "
                         "%s received, %s sent; uptime %s")
                (transmission-rate .downloadSpeed)
                (transmission-rate .uploadSpeed)
                .activeTorrentCount .torrentCount
                (transmission-size .current-stats.downloadedBytes)
                (transmission-size .current-stats.uploadedBytes)
                (transmission-eta .current-stats.secondsActive nil)))))
   "session-stats"))


(use-package transmission
  :ensure t
  :commands transmission-add
  :bind (("C-x t" . transmission)
         :map elfeed-show-mode-map
         ("C-c a t" . my-add-first-elfeed-enclosure-to-transmission)
         :map dired-mode-map
         ("C-c a t" . my-dired-add-to-transmission))
  :config (setq transmission-rpc-auth
                '(:username "transmission" :password "transmission")
                elfeed-enclosure-default-dir "~/torrent/"
                global-mode-string '(my-transmission-current-status display-time-string)))


(provide 'my-transmission)
;;; my-transmission.el ends here

