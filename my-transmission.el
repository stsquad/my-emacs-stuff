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

(defun my-add-first-elfeed-enclosure-to-transmission ()
  "Queue the first enclosure (if it is a torrent)."
  (interactive)
  (let ((enclosures (elfeed-entry-enclosures elfeed-show-entry)))
    (when (and enclosures
               (string-match-p
                "application/x-bittorrent"
                (nth 1 (-first-item enclosures))))
      (transmission-add (-first-item (-first-item enclosures))))))

(use-package transmission
  :ensure t
  :commands transmission-add
  :bind (("C-x t" . tranmission)
         :map elfeed-show-mode-map
         ("C-c a t" . my-add-first-elfeed-enclosure-to-transmission)
         :map dired-mode-map
         ("C-c a t" . my-dired-add-to-transmission))
  :config (setq transmission-rpc-auth
                '(:username "transmission" :password "transmission")
                elfeed-enclosure-default-dir "~/torrent/"))

(provide 'my-transmission)
;;; my-transmission.el ends here

