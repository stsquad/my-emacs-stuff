;;
;; my-emacs-server
;;
;
; Lets consider if we should start the server up. We also want to
; ensure new files appear in new  frames and bind C-c C-k to kill the
; client session without warning

(require 'server)

(message "In my-emacs-server")

;; is-emacs-server-running
;
; Check for a running server, we do this by looking for the server
; socket "/tmp/emacs${UID}/server"

(defun is-server-running ()
  "Check is an emacs-server process is already running"
  (let ((socket-path (concat server-socket-dir "/server")))
    (file-exists-p socket-path)))

;; Hooks
(defun my-server-switch-hook ()
  "My hook to run on server switch"
  (message "my-server-switch-hook")
  (if I-am-in-X
      (let ((server-buf (current-buffer)))
	(bury-buffer)
	(iconify-frame)
	(switch-to-buffer-other-frame server-buf)))
  (local-set-key (kbd "C-x C-c") 'server-edit)
  (local-set-key (kbd "C-x k") 'server-edit))

(defun my-server-done-hook ()
  "My hook when the server is done"
    (if I-am-in-X
	(delete-frame)))

(defun my-server-kill-emacs-hook ()
  "Clean up server files when we exit"
  (if (is-server-running)
      (delete-file (concat server-socket-dir "/server"))))

(unless (is-server-running)
      (server-start)
      
      ; kill new buffers when done
      (custom-set-variables '(server-kill-new-buffers t))
      
      ; hook functions
      (add-hook 'server-switch-hook 'my-server-switch-hook)
      (add-hook 'server-done-hook 'my-server-done-hook)
      (add-hook 'kill-emacs-hook 'my-server-kill-emacs-hook)

      ; minimise the frame
      (iconify-frame)
      
      (message "Started emacs-server"))


