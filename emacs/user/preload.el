;; * For SCIMAX starterkit, install scimax first.
(setq warning-minimum-level :emergency)
(setq package-enable-at-startup nil)

;; * Scimax
(add-hook 'org-mode-hook 'scimax-autoformat-mode)

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
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))


;; * Auto Install My Packages
(setq package-selected-packages
      '(material-theme
	gruvbox-theme
	doom-themes
	ef-themes
	;; srcery-theme
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
	;; company-auctex
	rjsx-mode
	tide
	web-mode
	emmet-mode
	;; rust-mode
	rustic
	;; company-web
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
	nov
	markdown-mode
	mixed-pitch ;;disable org-block-begin-line, org-block-end-line in .el file
	smartparens
	spice-mode
	ob-spice
	lsp-mode
	lsp-ui
	company
	jedi
	saveplace-pdf-view
	ag
	vertico
	which-key
	rg
	ob-rust
	lua-mode
	direnv
	magik-mode
	treemacs
	lsp-treemacs
	))
(package-install-selected-packages)

;; * Pyvenv
(require 'pyvenv)
(pyvenv-activate "~/Downloads/NixOS/python/.devenv/state/venv/")


;; * direnv + lspbridge
(use-package envrc
  :ensure t
  :delight 'envrc-mode
  :init
  (advice-add 'lsp :before #'direnv-update-environment)
  ;; (add-hook 'direnv-after-update-hook (lambda ()
  ;; 					(when (bound-and-true-p lsp-bridge-mode)
  ;;                                         (lsp-bridge-restart-process))))
  :config
  (envrc-global-mode +1)
  )

;; * lsp-bridge-mode
;; (add-to-list 'load-path "~/Downloads/lsp-bridge")
;; (add-to-list 'load-path "~/Downloads/flymake-bridge")

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'flymake-bridge)
;; (add-hook 'lsp-bridge-mode-hook #'flymake-bridge-setup)

;; (setq lsp-bridge-enable-org-babel t)

;; (setq lsp-bridge-org-babel-lang-list '(python rust javascript))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-to-list 'lsp-bridge-completion-popup-predicates
;; 			 (lambda ()
;; 			   (org-in-src-block-p)))))


;; * lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
	 (rustic-mode . lsp)
	 (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))


;; * lsp-bridge for reference only
;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;; 			 :files (:defaults "*.el" "*.py" "acm" "core"
;; 					   "langserver" "multiserver" "resources")
;; 			 :build (:not compile))
;;   :ensure nil 
;;   :hook
;;   (org-mode . lsp-bridge-mode)
;;   ;; Ensure src-edit buffers (C-c ') get lsp-bridge
;;   (org-src-mode . (lambda () (lsp-bridge-mode 1)))
;;   :init
;;   (global-lsp-bridge-mode)
;;   (setq lsp-bridge-enable-diagnostics t
;;         lsp-bridge-enable-signature-help t
;;         lsp-bridge-enable-hover-diagnostic t
;;         lsp-bridge-enable-auto-format-code nil
;;         lsp-bridge-enable-completion-in-minibuffer nil
;;         lsp-bridge-enable-log t
;;         lsp-bridge-org-babel-lang-list nil
;;         lsp-bridge-enable-org-babel t
;;         lsp-bridge-use-popup t
;;         ;; lsp-bridge-python-lsp-server "pylsp"
;; 	;; lsp-bridge-nix-lsp-server "nil"
;; 	;; lsp-bridge-tex-lsp-server "texlab"
;;         ;; lsp-bridge-csharp-lsp-server "omnisharp-roslyn"
;; 	))

;; (with-eval-after-load 'org
;;   (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
;;   ;; (add-to-list 'org-src-lang-modes '("jupyter-R" . ess-r))
;;   (add-to-list 'org-src-lang-modes '("rust" . rustic)))

