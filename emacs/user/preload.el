;; * For SCIMAX starterkit, install scimax first.
(setq warning-minimum-level :emergency)

;; * Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/tmp/emacs/undo")))
;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too


;; * Treat all thmes as safe
(setq custom-safe-themes t) 
(setq scimax-theme nil)

;; * Load MELPA
(when (>= emacs-major-version 24)
  (progn
    ;; load emacs 24's package system.
    (require 'package)
    ;; Add MELPA repository.
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (when (< emacs-major-version 27) (package-initialize)))


;; * Auto Install My Packages
(setq package-selected-packages
      '(material-theme
        neotree
        all-the-icons
        rainbow-delimiters
        yaml-mode
        dockerfile-mode
        toml-mode
        dumb-jump
        json-mode
	prettier-js
	js2-refactor
	company-auctex
	rjsx-mode
	tide
	web-mode
	emmet-mode
	company-web
	ox-rst
	alert
	org-superstar
	ob-nix
	latex-preview-pane
	org-modern
	slime
	nix-mode
	racket-mode
	geiser-mit
	))
(package-install-selected-packages)

;; ** Load Custom Directory Recursively
(let ((default-directory "~/NixOS/emacs/user/custom/"))
  (normal-top-level-add-subdirs-to-load-path))
