;;
;; my-emacs-server
;;
;
; Lets consider if we should start the server up. We also want to
; ensure new files appear in new  frames and bind C-c C-k to kill the
; client session without warning

(require 'server)

(message "In my-emacs-server")

(defvar my-server-frame
  (car (frame-list))
  "The *master* frame belonging to the server")


; interogate the window manager
(defun interogate-wm ()
  "Find out where in the window manager we are"
  (let ((clients server-buffer-clients)
	(pos (shell-command-to-string "wmctrl -d")))
    (message "wmctrl: %s" pos)
    (message "Clients: %s" clients)))

;(interogate-wm)
;(message "%s" (process-list (car  server-buffer-clients)) "env"))
;(process-name (car server-buffer-clients))


;; is-emacs-server-running
;
; Check for a running server, we do this by looking for the server
; socket "/tmp/emacs${UID}/server"

(defun is-server-running ()
  "Check is an emacs-server process is already running"
  (interactive)
  (let ((socket-path (concat server-socket-dir "/server")))
    (if (functionp 'server-running-p)
	(server-running-p socket-path)
      ; fall back, not as reliable
      (file-exists-p socket-path))))

;; Hooks
;
; When we enter the server hook we may want to do some funny stuff in
; X. It seems there is a difference between "emacsclient -c" and a
; script calling "emacsclient -c". Certainly without modern
; emacsclients we may need to create the new frame

(defun my-server-switch-hook ()
  "My hook to run on server switch"
  (message "my-server-switch-hook: %s" (current-buffer))
  (message " frame is: %s" (frame-parameter nil 'name))
  (if I-am-in-X
      (let ((server-buf (current-buffer)))
	; This works around the occasions emacs doesn't
	; switch you to the newly created frame
	(if (equal (selected-frame) my-server-frame)
	    (progn
	      (message "  hmm, not in the new frame")
	      (bury-buffer)
;	      (delete-other-frames)
	      (switch-to-buffer-other-frame server-buf)
	      (iconify-frame my-server-frame)))))

  ; Bind the keys for the buffer
  (local-set-key (kbd "C-x C-c") 'server-edit)
  (local-set-key (kbd "C-x k") 'server-edit))

(defun my-server-done-hook ()
  "My hook when the server is done"
    (if I-am-in-X
	(unless (equal (selected-frame) my-server-frame)
	  (delete-frame))))


(defun my-server-kill-emacs-hook ()
  "Clean up server files when we exit"
  (if (is-server-running)
      (delete-file (concat server-socket-dir "/server"))))


;;
;; my-server-start
;;
; Spawn the emacs server

(defun my-server-start ()
  "Start up an emacs server"
  (interactive)
  (server-start)

  ; We don't want desktop for the server process
  (desktop-save-mode 0)
      
  ; kill new buffers when done
  (custom-set-variables '(server-kill-new-buffers t))
      
  ; hook functions
  (add-hook 'server-switch-hook 'my-server-switch-hook)
  (add-hook 'server-done-hook 'my-server-done-hook)
  (add-hook 'kill-emacs-hook 'my-server-kill-emacs-hook)

  ; minimise the frame
  (set-frame-name "server frame")
  (iconify-frame my-server-frame)
  (message "Started emacs-server"))


