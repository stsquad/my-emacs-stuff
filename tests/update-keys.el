;;
;; Update the GNU Elpa Keys
;;

(package-initialize)
(package-refresh-contents)
(package-install 'gnu-elpa-keyring-update)
(gnu-elpa-keyring-update)
