;;; my-paths --- Basic load-path set-up
;;
;;; Commentary:
;;
;; This used to sit at the start of my init.el but I split it out as
;; it's useful to have the functions for Emacs -Q invocations.
;;
;;; Code:

(defvar my-config-root
  "~/.emacs.d"
  "Where all my config files are kept.")

(defvar my-config-paths
  '("~/.emacs.d/my-elisp" "~/.emacs.d/my-local-pkgs")
  "Additional search paths I use.")

(defun my-add-config-paths (&optional paths)
  "Add `PATHS' to the load-path. If `PATHS' is not specified default
  to the value of `my-config-paths'."
  (mapc #'(lambda (p)
            (when (and (file-exists-p p)
                       (file-directory-p p))
              (add-to-list 'load-path p)))
        (or paths my-config-paths))
  (message "load-path is: %s" load-path))

(defun my-add-git-project-paths (&optional root)
  "Add any .git directories in `ROOT' to the `load-path'."
  (let ((dirs (directory-files (or root my-config-root) 't "\.git$")))
    (mapc #'(lambda (f)
              (let ((default-directory f))
                (setq load-path
                      (append
                       (let ((load-path (copy-sequence load-path))) ;; Shadow
                         (append
                          (copy-sequence (normal-top-level-add-to-load-path '(".")))
                          (normal-top-level-add-subdirs-to-load-path)))
                       load-path)))) dirs)
    (setq load-path (delete-dups load-path))))

(provide 'my-paths)
;;; my-paths.el ends here

