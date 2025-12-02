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


;; ;; ** Pyvenv
;; (require 'pyvenv)
;; (pyvenv-activate "~/.virtualenvs/tf/")

;; ** Scimax
(add-hook 'org-mode-hook 'scimax-autoformat-mode)
;; (add-hook 'org-mode-hook 'direnv)

;; * direnv + lspbridge
;; Configure and load the emacs-direnv package
;; (use-package direnv
;;   :ensure t
;;   :config
;;   ;; Enable global direnv mode
;;   (direnv-mode 1)

;;   )

(use-package envrc
  :ensure t
  ;; :after lsp
  ;; :delight 'envrc-mode
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  (add-hook 'before-hack-local-variables-hook #'direnv-update-environment)
  (advice-add 'python-mode :before #'direnv-update-environment)
  (add-hook 'prog-mode-hook #'direnv--maybe-update-environment)
  (advice-add 'lsp :before #'direnv-update-environment)
  (add-hook 'prog-mode-hook
            (lambda () (progn (direnv-update-environment) (lsp))))
  :config
  (envrc-global-mode +1)
  )

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;; 			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;; 			 :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode))

(add-to-list 'load-path "~/Downloads/lsp-bridge")
(add-to-list 'load-path "~/Downloads/flymake-bridge")

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require 'flymake-bridge)
(add-hook 'lsp-bridge-mode-hook #'flymake-bridge-setup)




;; (advice-add 'lsp :before #'direnv-update-environment)


;; (require 'lsp-mode)

;; (require 'company)

;; ;; Enable company-mode globally or in rustic-mode hook
;; (add-hook 'after-init-hook 'global-company-mode)

;; ;; Optional: Configure company-mode for better integration
;; (setq company-idle-delay 0.1) ; Shorter delay for autocompletion
;; (setq company-minimum-prefix-length 2) ; Minimum characters before autocompletion starts
;; (setq lsp-completion-provider :company) ; Ensure company is the completion provider
;; (setq lsp-enable-file-watchers nil)
;; (setq lsp-auto-guess-root nil)


;; (advice-add 'org-mode :before #'direnv-update-environment)
