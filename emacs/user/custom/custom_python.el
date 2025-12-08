;; cumtom_python.el

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(add-hook 'python-mode-hook #'lsp-bridge-mode)

;; (add-hook 'python-mode-hook #'lsp)
;; (setq lsp-pyright-auto-search-paths t)
;; (setq lsp-pyright-langserver-command "basedpyright")
;; (setq lsp-bridge-python-lsp-server "basedpyright")
;; (setq lsp-bridge-python-lsp-server "ruff")
;; (setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff")


;; (lsp-bridge-register-server
;;  '(python-ruff
;;    :language "python"
;;    :server-id "ruff"
;;    :command ("ruff-lsp") ; Or "ruff" if you're using the native Rust server
;;    :initialization-options nil))

;; ;; setup PATH for remote command execution
;; (with-eval-after-load 'tramp
;;   (add-to-list 'tramp-remote-path "~/.nix-profile/bin")
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; (use-package apheleia
;;   :config
;;   ;; which formatter to use
;;   (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
;;   (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
;;   ;; don't mess up with lsp-mode
;;   (setq +format-with-lsp nil)
;;   ;; run the formatter inside container
;;   (setq apheleia-remote-algorithm 'remote))
