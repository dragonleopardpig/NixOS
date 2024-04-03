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

;; Org Modern Mode
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; ;; Kaolin Themes
;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-galaxy t)
;;   (kaolin-treemacs-theme))

;; Heaven and Hell
(use-package heaven-and-hell
  :ensure t
  :config
  (setq heaven-and-hell-theme-type 'light)
  (setq heaven-and-hell-themes
        '((light . (dichromacy doom-acario-light doom-plain leuven))
          (dark . (doom-oceanic-next doom-plain-dark misterioso doom-badger leuven-dark))
	  ))
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
;; 		    ("python" (:background "PaleVioletRed4" :extend t))
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
;; 		  ("jupyter-python" (:background "thistle1" :extend t))
;; 		  ("ipython" (:background "thistle1" :extend t))
;; 		  ("python" (:background "DarkSeaGreen1" :extend t))
;; 		  ("sqlite" (:background "#a24224" :extend t))
;; 		  ("haskell" (:background "#4200a2" :extend t))
;; 		  ("nix" (:background "maroon" :extend t))
;; 		  ("lisp" (:background "honeydew" :extend t))))
;; 	  (custom-set-faces
;; 	   '(org-block-begin-line
;; 	     ((t (
;; 		  :underline nil
;; 		  :overline nil
;; 		  :foreground "indian red"
;; 		  :background "white"
;; 		  :italic t
;; 		  :bold t
;; 		  :extend t))))
;; 	   '(org-block-end-line
;; 	     ((t (
;; 		  :underline nil
;; 		  :overline nil
;; 		  :foreground "indian red"
;; 		  :background "white"
;; 		  :italic t
;; 		  :bold t
;; 		  :extend t))))
;; 	   '(org-level-1 ((t (:foreground "indian red")))))))
;;       ;; ##################################
;;       (dolist (theme themes)
;;         ;; (when heaven-and-hell-load-theme-no-confirm
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

