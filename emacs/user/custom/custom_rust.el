;; Rustic Mode
(require 'rustic)

;; (add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'rustic-mode-hook 'lsp-deferred)

(setenv "PATH" (concat (getenv "PATH") ":" "/home/thinky/Downloads/NixOS/rust/.devenv/profile/bin"))
(add-to-list 'exec-path "/home/thinky/Downloads/NixOS/rust/.devenv/profile/bin")

;; ;; (setq rustic-analyzer-command '("/nix/store/2885whsz3962pvsifx6kx0in2dh4rhym-rust-complete-1.91.1/bin/rust-analyzer"))
;; ;; (setq rustic-rustfmt-bin '("/nix/store/2885whsz3962pvsifx6kx0in2dh4rhym-rust-complete-1.91.1/bin/rustfmt"))

;; ;; 1. Tell Org to use rustic-mode instead of rust-mode/fundamental-mode
;; (with-eval-after-load 'org
;;   (add-to-list 'org-src-lang-modes '("rust" . rustic)))

;; ;; 2. Enable lsp-bridge whenever rustic opens
;; (add-hook 'rustic-mode-hook #'lsp-bridge-mode)

;; ;; 3. Ensure org-src edit buffer activates rustic-mode + LSP-Bridge
;; (defun my/org-rust-src-setup ()
;;   (when (and (string= (format "%s" major-mode) "rustic-mode"))
;;     (lsp-bridge-mode 1)))

;; (add-hook 'org-src-mode-hook #'my/org-rust-src-setup)
