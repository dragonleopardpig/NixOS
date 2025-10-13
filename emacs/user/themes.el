;; * Disable some defaults
(disable-theme 'smart-mode-line-light)
(google-this-mode -1)
(fringe-mode -1)

;; * Set Faces, etc...
(set-face-attribute 'default nil :height 100)
(setq leuven-scale-outline-headlines 1.1)
(setq text-scale-mode-step 1.05)
(setq org-indent-indentation-per-level 0)
(global-visual-line-mode t)
;; (global-display-fill-column-indicator-mode t)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook
;; 	  (lambda() (set-fill-column 80)))
(column-number-mode)
;; (add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setopt display-fill-column-indicator-column 80)

;; * Custom Keyboard Shortcut
(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)
(global-set-key (kbd "C-M-a") 'org-babel-mark-block)

;; * GUI Interface
(delete-selection-mode 1)
(desktop-save-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Doom Themes
(use-package doom-themes
  :ensure t
  :config
  ;; :tools direnv
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  ;; ;; use "doom-colors" for less minimal icon theme
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-thexmes-treemacs-config)
  )

;; * Org Modern
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

;; Option 2: Globally
(with-eval-after-load 'org (global-org-modern-mode))

;; * Heaven and Hell
(use-package heaven-and-hell
  :ensure t
  :config
  (setq heaven-and-hell-theme-type 'light)
  (setq heaven-and-hell-themes
        '((light . (doom-acario-light dichromacy doom-plain leuven doom-fairy-floss))
          (dark . (doom-oceanic-next doom-plain-dark misterioso doom-badger leuven-dark))))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f8>" . heaven-and-hell-load-default-theme)
         ("<f8>" . heaven-and-hell-toggle-theme)))
