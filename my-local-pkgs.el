;;; my-local-pkgs.el --- Setup for locally installed packages
;;
;;; Commentary:
;;
;; This sets up any locally installed packages (i.e. not distributed
;; through ELPA/MELPA) which all sit in ~/.emacs.d/my-local-pkgs/
;;
;;; Code:
;;
(require 'use-package)
(require 'my-utils)
(require 'my-tracking)

;; currently in my branch of RISU
(use-package risu
  :mode ("\\.risu\\'" . risu-mode))
  
;; QEMU system mode comint mode (https://github.com/stsquad/qemu-mode)
(use-package qemu-mode
  :commands run-qemu
  :config
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
(use-package xml-rpc)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package lava-rpc
  :commands lava-xml-rpc-call
  :config
  (setq lava-user-name "alex.bennee"
        lava-api-token #'(lambda () (my-pass-password "lava" t))))

(use-package lava-job-list-mode
  :bind ("C-c l" . lava-list-jobs)
  :config
  (progn
    (when (fboundp 'tracking-add-buffer)
      (add-to-list 'lava-job-list-mode-hook
                   #'(lambda ()
                       (add-to-list 'tabulated-list-revert-hook
                                    #'(lambda ()
                                        (tracking-add-buffer (current-buffer)))))))))
(use-package lava-yaml-mode
  :mode (("lava-mode.*\\.yaml$" . lava-yaml-mode)
         ("test-runners.*\\.yaml$" . lava-yaml-mode)))

(use-package lava-json-mode
  :mode (("lava-mode.*\\.json$" . lava-json-mode)
         ("test-runners.*\\.json$" . lava-json-mode))
  :config
  (progn
    (setq-default
     lava-mode-test-repository
     "http://git.linaro.org/people/alex.bennee/test-definitions.git"
     lava-mode-default-bundle-stream "/anonymous/alex.bennee/")
    (setq lava-mode-default-device-image-alist
          '(("arndale" . (("command" . "deploy_linaro_image")
                          ("metadata" . (("rootfs.type" . "server")
                                         ("ubuntu.build" . "618")
                                         ("ubuntu.distribution" . "ubuntu")
                                         ("ubuntu.name" . "arndale")))
                          ("parameters" . (("image" .
                                            "http://snapshots.linaro.org/ubuntu/pre-built/arndale/618/arndale-saucy_server_20140325-618.img.gz")))))
            ("kvm" . (("command" ."deploy_linaro_image")
                      ("parameters" . (("image" .
                                        "http://people.linaro.org/~matthew.hart/images/trusty-lg.img.gz")))))
            ("mustang" . (("qemu-arch" . "aarch64")
                          ("command" . "deploy_linaro_kernel")
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
                                                ("rootfs" . "http://snapshots.linaro.org/ubuntu/images/developer/650/linaro-saucy-developer-20140325-650.tar.gz")))))))))

(provide 'my-local-pkgs)
;;; my-local-pkgs ends here
