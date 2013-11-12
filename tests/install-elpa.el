;;
;; ELPA Package installer
;;

; from: http://tromey.com/elpa/install.html
(defun my-install-elpa ()
  "Load and run the ELPA package installer"
  (let ((buffer (url-retrieve-synchronously
                 "http://tromey.com/elpa/package-install.el")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
    (kill-buffer (current-buffer)))))
