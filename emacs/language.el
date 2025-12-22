;; * direnv + lspbridge
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode +1)
  )

;; * General Setting (disable some lsp features)
(setq jinx-languages "en_US")
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-completion-provider :none)
(setq lsp-completion-show-detail nil)
(setq lsp-completion-show-kind nil)

(setenv "PATH" (concat (getenv "PATH") ":" "~/Downloads/NixOS/rust/.devenv/profile/bin"))
(add-to-list 'exec-path "~/Downloads/NixOS/rust/.devenv/profile/bin")

(with-eval-after-load 'scimax-yas
  (yas-global-mode -1))

;; * lsp-bridge-mode
(add-to-list 'load-path "~/Downloads/lsp-bridge")
(add-to-list 'load-path "~/Downloads/flymake-bridge")

(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-enable-completion-in-string nil)
(setq acm-enable-search-file-words nil)

(require 'yasnippet)
(yas-global-mode 1)

(require 'flymake-bridge)
(add-hook 'lsp-bridge-mode-hook #'flymake-bridge-setup)

;; * Rust
(require 'lsp-bridge-rust)

;; * Python
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; * lsp-bridge-mode for python
;; (add-hook 'python-mode-hook #'lsp-bridge-mode)
(setq lsp-bridge-python-lsp-server "basedpyright")
;; (setq lsp-bridge-python-lsp-server "ruff")
(setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff")

