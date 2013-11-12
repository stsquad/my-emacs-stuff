;;
;; htmlize
;;

(require 'htmlize)

(setq htmlize-output-type 'inline-css)

; From http://ruslanspivak.com/2007/08/18/htmlize-your-erlang-code-buffer/
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
