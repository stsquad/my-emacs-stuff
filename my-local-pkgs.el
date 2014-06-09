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
  (global-set-key (kbd "C-c l") 'lava-list-jobs)
  (setq lava-user-name "ajbennee"
        lava-api-token (my-pass-password "lava")
        lava-mode-default-device-image-alist
        '(("arndale" . (("command" . "deploy_linaro_image")
                        ("metadata" . (("rootfs.type" . "server")
                                       ("ubuntu.build" . "618")
                                       ("ubuntu.distribution" . "ubuntu")
                                       ("ubuntu.name" . "arndale")))
                        ("parameters" . (("image" .
                                          "http://snapshots.linaro.org/ubuntu/pre-built/arndale/618/arndale-saucy_server_20140325-618.img.gz")))))
          ("kvm" . (("command" ."deploy_linaro_image")
                    ("parameters" . (("image" .
                                      "http://community.validation.linaro.org/images/kvm/ubuntu-12-04-server-base-lava.img.gz")))))
          ("mustang" . (("command" . "deploy_linaro_kernel")
                        ("parameters" . (("dtb" .
                                          "http://images-internal/mustang/mustang.dtb")
                                         ("kernel" .
                                          "http://images-internal/mustang/uImage")
                                         ("nfsrootfs" .
                                          "http://people.linaro.org/~alex.bennee/images/trusty-core-lava.tar.gz")))))
          ("arndale-octa" . (("command" . "deploy_linaro_image")
                             ("metadata" . (( "distribution" . "ubuntu" )
                                            ( "hwpack.build" . "20" )
                                            (  "hwpack.type" . "arndale-octa" )
                                            (  "rootfs.build". "650" )
                                            (  "rootfs.type" . "developer" )))
                              ("parameters" . (( "hwpack" .
                                                 "http://snapshots.linaro.org/kernel-hwpack/linux-lisa-arndale-octa/20/hwpack_linaro-arndale-octa_20140404-2235_b20_armhf_supported.tar.gz")
                                               ("rootfs" . "http://snapshots.linaro.org/ubuntu/images/developer/650/linaro-saucy-developer-20140325-650.tar.gz")))))))
  (setq-default lava-mode-test-repository
                "http://git.linaro.org/people/alex.bennee/test-definitions.git")
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
