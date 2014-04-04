;;; my-local-pkgs.el --- Setup for locally installed packages
;;
;;; Commentary:
;;
;; This sets up any locally installed packages (i.e. not distributed
;; through ELPA/MELPA) which all sit in ~/.emacs.d/my-local-pkgs/
;;
;;; Code:
;;
(require 'my-utils)

;; currently in my branch of RISU
(require 'risu nil t)

;; QEMU system mode comint mode (https://github.com/stsquad/qemu-mode)
(when (require 'qemu-mode nil t)
  (setq qemu-executable-path
        "/home/alex/lsrc/qemu/qemu.git/aarch64-softmmu/qemu-system-aarch64"
        qemu-kernel-image
        "/home/alex/lsrc/qemu/linux.git/arch/arm64/boot/Image"
        qemu-kernel-params
        "console=ttyAMA0 debug init=/bin/init"
        qemu-machine-params
        "-cpu cortex-a57 -machine type=virt -nographic -smp 1 -m 512"
        qemu-net-device-params
        nil
        qemu-drive-device-params
        nil))

;; LAVA mode
(when (require 'lava-mode nil t)
  (setq lava-user-name "ajbennee"
        lava-api-token (my-pass-password "lava"))
  (add-to-list 'auto-mode-alist '("lava-mode.*\\.json$" . lava-mode))
  (when (require 'tracking nil 'noerror)
    (add-to-list
     'lava-job-list-mode-hook
     #'(lambda ()
         (add-to-list
          'tabulated-list-revert-hook
          #'(lambda ()
              (tracking-add-buffer (current-buffer))))))))

(provide 'my-local-pkgs)
;;; my-local-pkgs ends here
