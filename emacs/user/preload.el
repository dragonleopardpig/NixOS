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


;; * Treat all themes as safe
(setq custom-safe-themes t) 
(setq scimax-theme nil)

;;* Load MELPA
;; Initialize package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))


;; * Auto Install My Packages
(setq package-selected-packages
      '(material-theme
	gruvbox-theme
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
	rust-mode
	company-web
	ox-rst
	alert 
	org-fragtog
	ob-nix
	latex-preview-pane
	org-modern
	slime
	nix-mode
	geiser-mit
	pyvenv
	srcery-theme
	nov
	markdown-mode
	mixed-pitch ;;disable org-block-begin-line, org-block-end-line in .el file
	smartparens
	spice-mode
	ob-spice
	lsp-mode
	jedi
	saveplace-pdf-view
	ag
	vertico
	which-key
	rg
	ob-rust
	lua-mode
	))
(package-install-selected-packages)


;; ** Pyvenv
(require 'pyvenv)
(pyvenv-activate "~/.virtualenvs/tf/")

;; ** Scimax
(add-hook 'org-mode-hook 'scimax-autoformat-mode)

;; * Direnv + LSP + Company
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(require 'lsp-mode)

(require 'company)

;; Enable company-mode globally or in rustic-mode hook
(add-hook 'after-init-hook 'global-company-mode)

;; Optional: Configure company-mode for better integration
(setq company-idle-delay 0.1) ; Shorter delay for autocompletion
(setq company-minimum-prefix-length 2) ; Minimum characters before autocompletion starts
(setq lsp-completion-provider :company) ; Ensure company is the completion provider
(setq lsp-enable-file-watchers nil)
(setq lsp-auto-guess-root nil)

(advice-add 'lsp :before #'direnv-update-environment)
