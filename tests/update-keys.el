;;
;; Update the GNU Elpa Keys
;;

(package-initialize)
(package-refresh-contents)
(setq package-check-signature nil)
(package-install 'gnu-elpa-keyring-update)
(gnu-elpa-keyring-update)
