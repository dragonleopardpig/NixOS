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


;; ;; * Fix
;; (require 'lsp-bridge)
;; (acm-mode 1)
;; (setq lsp-bridge-enable-auto-completion t)

;; (setq org-src-lang-modes '(("rust" . rust)))

;; (defun my/org-rust-create-temp-file ()
;;   "Create a real Rust file for the current Org src block and open it."
;;   (interactive)
;;   (when-let* ((element (org-element-at-point))
;;               ((eq (org-element-type element) 'src-block))
;;               (lang (org-element-property :language element))
;;               (tangle-file (or (org-element-property :tangle element)
;;                                ;; fallback to src/temp.rs
;;                                "src/temp.rs")))
;;     ;; Ensure it’s Rust
;;     (when (string= lang "rust")
;;       ;; Expand path relative to Org file
;;       (let ((file-path (expand-file-name tangle-file (file-name-directory (buffer-file-name)))))
;;         ;; Create directory if it doesn’t exist
;;         (make-directory (file-name-directory file-path) t)
;;         ;; Create file if it doesn’t exist
;;         (unless (file-exists-p file-path)
;;           (write-region "" nil file-path))
;;         ;; Open file and enable LSP
;;         (find-file file-path)
;;         (lsp-bridge-mode 1)
;;         (acm-mode 1)
;;         (setq-local lsp-bridge-enable-auto-completion t)
;;         (message "Opened Rust file: %s" file-path)))))


;; ;; Bind to C-c '
;; (defun my/org-rust-edit-temp ()
;;   "Open a temporary Rust file for Org src editing."
;;   (interactive)
;;   (my/org-rust-create-temp-file))

;; (define-key org-mode-map (kbd "C-c '") #'my/org-rust-edit-temp)

;; (defun my/org-rust-open-file ()
;;   "Open the real Rust file of the current Org src block."
;;   (interactive)
;;   (let* ((element (org-element-at-point))
;;          (file (when (eq (org-element-type element) 'src-block)
;;                  (org-element-property :tangle element))))
;;     (if (and file (not (string= file "no")))
;;         (find-file (expand-file-name file (file-name-directory (buffer-file-name))))
;;       (message "Block has no :tangle file."))))

;; (define-key org-mode-map (kbd "C-c C-x e") #'my/org-rust-open-file)


