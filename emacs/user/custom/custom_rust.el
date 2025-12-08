;; Rustic Mode
(require 'rustic)

;; (setenv "PATH" (concat (getenv "PATH") ":" "/nix/store/2885whsz3962pvsifx6kx0in2dh4rhym-rust-complete-1.91.1/bin"))
;; (add-to-list 'exec-path "/nix/store/2885whsz3962pvsifx6kx0in2dh4rhym-rust-complete-1.91.1/bin")

(setenv "PATH" (concat (getenv "PATH") ":" "/home/thinky/Downloads/NixOS/rust/.devenv/profile/bin"))
(add-to-list 'exec-path "/home/thinky/Downloads/NixOS/rust/.devenv/profile/bin")

;; (setq rustic-analyzer-command '("/nix/store/2885whsz3962pvsifx6kx0in2dh4rhym-rust-complete-1.91.1/bin/rust-analyzer"))
;; (setq rustic-rustfmt-bin '("/nix/store/2885whsz3962pvsifx6kx0in2dh4rhym-rust-complete-1.91.1/bin/rustfmt"))
