;; * Disable some defaults
(disable-theme 'smart-mode-line-light)
(google-this-mode -1)

;; * Set Faces, etc...
(set-face-attribute 'default nil :height 110)
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

;; Kaolin Themes
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-galaxy t)
  (kaolin-treemacs-theme))

;; Heaven and Hell
(use-package heaven-and-hell
  :ensure t
  :config
  (setq heaven-and-hell-theme-type 'light)
  (setq heaven-and-hell-themes
        '((light . (doom-acario-light dichromacy doom-plain leuven))
          (dark . (doom-oceanic-next doom-plain-dark misterioso doom-badger leuven-dark))))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

;; ;; Overwrite below in heaven-and-hell.el
;; (defun heaven-and-hell-clean-load-themes (theme-or-themes)
;;   "Load themes if they're not loaded yet and enable them cleanly.
;; Cleanly means that it disables all custom themes before enabling new ones.
;; THEME-OR-THEMES can be single theme or list of themes.
;; Themes will be loaded if they weren't loaded previously."
;;   (heaven-and-hell-load-default-theme)
;;   (when theme-or-themes
;;     (let ((themes
;; 	   (if (listp theme-or-themes) theme-or-themes `(,theme-or-themes))))
;;       ;; ## ChuPL
;;       (if (eq heaven-and-hell-theme-type 'dark)
;; 	  (progn
;; 	    (setq org-src-block-faces
;; 		  '(("emacs-lisp" (:background "LightCyan1" :extend t))
;; 		    ("sh" (:background "#2C001E" :extend t))
;; 		    ("jupyter-python" (:background "#001f26" :extend t))
;; 		    ("ipython" (:background "HotPink4" :extend t))
;; 		    ("python" (:background "#1d2100" :extend t))
;; 		    ("sqlite" (:background "#a24224" :extend t))
;; 		    ("haskell" (:background "#4200a2" :extend t))
;; 		    ("nix" (:background "maroon" :extend t))
;; 		    ("lisp" (:background "#232627" :extend t))))
;; 	    (custom-set-faces
;; 	     '(org-block-begin-line
;; 	       ((t (
;; 		    :underline nil
;; 		    :overline nil
;; 		    :foreground "chartreuse"
;; 		    :background "#232627"
;; 		    :italic t
;; 		    :bold t
;; 		    :extend t))))
;; 	     '(org-block-end-line
;; 	       ((t (
;; 		    :underline nil
;; 		    :overline nil
;; 		    :foreground "chartreuse"
;; 		    :background "#232627"
;; 		    :italic t
;; 		    :bold t
;; 		    :extend t))))
;; 	     '(org-level-1 ((nil )))))
;; 	(progn
;; 	  (setq org-src-block-faces
;; 		'(("emacs-lisp" (:background "LightCyan1" :extend t))
;; 		  ("sh" (:background "gray90" :extend t))
;; 		  ("jupyter-python" (:background "ivory" :extend t))
;; 		  ("ipython" (:background "thistle1" :extend t))
;; 		  ("python" (:background "DarkSeaGreen1" :extend t))
;; 		  ("sqlite" (:background "#a24224" :extend t))
;; 		  ("haskell" (:background "#4200a2" :extend t))
;; 		  ("nix" (:background "light yellow" :extend t))
;; 		  ("lisp" (:background "honeydew" :extend t))))
;; 	  (custom-set-faces
;; 	   '(org-block-begin-line
;; 	     ((t (
;; 		  :underline nil
;; 		  :overline nil
;; 		  :foreground "orange red"
;; 		  :background "white"
;; 		  :italic t
;; 		  :bold t
;; 		  :extend t))))
;; 	   '(org-block-end-line
;; 	     ((t (
;; 		  :underline nil
;; 		  :overline nil
;; 		  :foreground "orange red"
;; 		  :background "white"
;; 		  :italic t
;; 		  :bold t
;; 		  :extend t))))
;; 	   '(org-level-1 ((t (:foreground "red3")))))))
;;       ;; ##################################
;;       (dolist (theme themes)
;; 	;; (when heaven-and-hell-load-theme-no-confirm
;; 	(when heaven-and-hell-load-theme-no-confirm
;; 	  (load-theme theme t t)))
;;       (custom-set-variables `(custom-enabled-themes (quote ,themes)))
;;       ;; ** ChuPL
;;       (if (eq heaven-and-hell-theme-type 'dark)
;; 	  (progn (setq heaven-and-hell-theme-type 'dark)
;; 		 (set-background-color "#232627"))
;; 	(progn (setq heaven-and-hell-theme-type 'light)
;; 	       (set-background-color "white")))
;;       (org-mode-restart)
;;       (outline-show-children)
;;       (outline-show-entry)
;;       ;; *********
;;       )))



;; ;; Minimal UI
;; (package-initialize)
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (modus-themes-load-operandi)

;; ;; Choose some fonts
;; ;; (set-face-attribute 'default nil :family "Iosevka")
;; ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; ;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

;; (setq
;;  ;; Edit settings
;;  org-auto-align-tags nil
;;  org-tags-column 0
;;  org-catch-invisible-edits 'show-and-error
;;  org-special-ctrl-a/e t
;;  org-insert-heading-respect-content t

;;  ;; Org styling, hide markup etc.
;;  org-hide-emphasis-markers t
;;  org-pretty-entities t
;;  org-agenda-tags-column 0
;;  org-ellipsis "…")

;; (global-org-modern-mode)
