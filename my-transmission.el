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
;; 
;;
;;; Code:

;; Require prerequisites
(require 'use-package)

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
    (transmission-add (my-narrow-to-pure-magnet) t)))

(defun my-enable-torrent-snarfing ()
  "Bind torrent snarfing to action key."
  (interactive)
  (local-set-key (kbd "C-c C-c") 'my-snarf-magnet))

(use-package transmission
  :ensure t
  :commands transmission-add
  :config
  (setq transmission-rpc-auth '(:username "transmission" :password "transmission")))
  

(provide 'my-transmission)
;;; my-transmission.el ends here

