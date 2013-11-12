;;
;; my-project.el
;;
;; Simple project handling code: DEPRECATED


;; Define a current-project-root
;
; In most cases I open emacs at the top of a src tree. Searches and compiles
; can then use this as a starting point
;
; change with (setq current-project-root (chomp (shell-command-to-string "pwd")))
(defvar current-project-root
  (concat (chomp (shell-command-to-string "pwd")))
  "Describes the current project root dir (top of src tree)")

(defun set-project-root ()
  "Set a primary project root"
  (interactive)
  (setq current-project-root
	(read-file-name "Project Root:"))
  (global-set-key (kbd "C-c c") 'my-first-compile))

;; secondary-project-root
;
; Sometimes your working with two source trees. The secondary tree is
; tracked and useful for bringing up ediff's and the like
;

(defvar secondary-project-root
  ()
  "Describes a secondary project root (say for quick ediff)")

(defun set-secondary-root ()
  "Set a secondary project root"
  (interactive)
  (setq secondary-project-root
	(read-file-name "Secondary Project Root:")))

(defun set-secondary-root-if-not ()
  "Check is secondary-project-root is defined, if not set it"
  (if (not secondary-project-root)
      (set-secondary-root)))

;; my-first-compile
; Try and guess what we are doing and then call compile, we assume
; that any of the other modes will rebind the compile keys once done
(defun my-first-compile ()
  "First compile standard hook, decide what to do"
  (interactive)
  (message "my-first-compile: start")
  (cond
   ; Work
   ((file-exists-p (concat current-project-root "/build-system"))
    (load-library "cbnl"))
   ; Android
   ((file-exists-p (concat current-project-root
			   "/AndroidManifest.xml"))
    (load-library "my-android"))
   ; Rockbox
   ((file-exists-p (concat current-project-root "/rbutil"))
    (load-library "my-c-mode")
    (setq compile-command (format
			   "cd %s/../ipodsim.build && make -k"
			   current-project-root)))
   ; Generic Makefile driven C
   ((file-exists-p (concat current-project-root "/Makefile"))
    (load-library "my-c-mode"))
   (t
    (setq compile-command (format
			   "cd %s && make -k" current-project-root))))
  (compile (eval compile-command)))
		   
(global-set-key (kbd "C-c c") 'my-first-compile)
(global-set-key (kbd "<f3>")  'my-first-compile)


;; Search within a project
;
; We may have bound a more elegant solution to search earlier
;
(unless (global-key-binding (kbd "<f5>")
    ; otherwise fall-back
    (if (file-exists-p (concat current-project-root "/.git"))
        (progn
          (message "Using git grep for searches")
          (setq grep-command "git grep -n ")
          (global-set-key (kbd "<f5>") '(lambda (search)
                                          (interactive "sSearch string:")
                                          (cd current-project-root)
                                          (let ((buffer (concat "*git grep for " search "*" ))
                                                (command (concat "git grep -n " search)))
                                            (shell-command command buffer)
                                            (pop-to-buffer buffer)
                                            (grep-mode)))))
      ; otherwise a simple grep will have to do
      (global-set-key (kbd "<f5>") '(lambda (search)
                                      (interactive "sSearch string:")
                                      (grep-find (concat "find "
                                                         current-project-root
                                                         " -print0 |  xargs -0 -e grep -n -e "
                                                         search)))))))


; my-find-files (find a file name from project root)
;
; extends find-name-dired to use project root

;(defun my-find-files (filespec)
;  (interactive "sSearch file:")
;  (grep-find (concat "find " current-project-root " -iname '" filespec "*' -printf '%p:0:\n'")))

(defun my-find-files (filespec)
   (interactive "sSearch for filespec:")
   (find-name-dired current-project-root filespec))

(global-set-key (kbd "<f6>") 'my-find-files)


;; ediff tweaks

;; my-ediff-secondary
;
; See if we can find the equivilent file in a secondary project root
; and bring up ediff on it.

(defun my-ediff-secondary ()
  "Load the secondary equivilent file of this buffer and ediff it"
  (interactive)
					; sanity
  (set-secondary-root-if-not)
  (message (concat "buffer name is " (buffer-file-name)))
  (let
      (
       (primary-buffer (buffer-file-name))
       (secondary-buffer (replace-regexp-in-string
			  current-project-root secondary-project-root
			  (buffer-file-name))))
					; sanity
    (if (file-exists-p secondary-buffer)
	(ediff-files secondary-buffer primary-buffer)
      (ediff-files (read-file-name
		    (concat "Whats the path to " buffer-file-name))
		   primary-buffer))))
