;; Save this file in ~/.emacs.d/

(load  "/home/thinky/emacs/user/preload.el")
;; (load  "/home/thinky/mountdir/emacs/scimax/init.el")
(load  "/home/thinky/Downloads/scimax/init.el")
(load  "/home/thinky/emacs/user/user.el")
;; (load  "/home/thinky/emacs/user/themes.el")

;; Add your custom lisp directory to load-path
(add-to-list 'load-path "~/NixOS/emacs/user/custom/")

;; Load your custom files
;; (load "my-custom-file.el")
;; (load "another-custom-file.el")
;; Or, if you have many, you can iterate and load them
(mapc 'load (file-expand-wildcards "~/NixOS/emacs/user/custom/*.el"))
