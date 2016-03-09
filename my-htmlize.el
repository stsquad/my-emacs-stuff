;;; my-htmlize --- htmlize tweaks
;;
;; Copyright (C) 2014 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;;
;;; Commentary:
;;
;; I don't use this so much now but it is useful for posting codedumps
;; in a formatted way
;;
;;; Code

(require 'use-package)
(require 'my-web)

(use-package htmlize
  :if (locate-library "htmlize")
  :commands (htmlize-faces-in-buffer htmlize-make-face-map htmlize-css-specs htmlize-region-for-paste)
  :config
  (setq htmlize-output-type 'inline-css))

;; From http://ruslanspivak.com/2007/08/18/htmlize-your-erlang-code-buffer/
(defun my-htmlize-region (beg end)
  "Htmlize region and put into <pre> tag style that is left in <body> tag
plus add font-size: 8pt"
  (interactive "r")
  (let* ((buffer-faces (htmlize-faces-in-buffer))
         (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
         (pre-tag (format
                   "<pre style=\"%s font-size: 8pt\">"
                   (mapconcat #'identity (htmlize-css-specs
                                          (gethash 'default face-map)) " ")))
         (htmlized-reg (htmlize-region-for-paste beg end)))
    (switch-to-buffer-other-window "*htmlized output*")
                                        ; clear buffer
    (kill-region (point-min) (point-max))
                                        ; set mode to have syntax highlighting
    (web-mode)
    (save-excursion
      (insert htmlized-reg))
    (while (re-search-forward "<pre>" nil t)
      (replace-match pre-tag nil nil))
    (goto-char (point-min))))

(provide 'my-htmlize)
;;; my-htmlize.el ends here
