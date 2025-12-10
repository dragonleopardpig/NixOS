;; * Dashboard
(use-package dashboard
  :ensure t
  :config 
  (setq dashboard-startup-banner "~/Downloads/NixOS/emacs/Emacs-logo.xpm") ;convert svg to xpm
  ;; Other dashboard configurations
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)
			  (bookmarks . 10)
			  (projects . 5)
			  (agenda . 5)
			  (registers . 5)))
  (setq dashboard-set-navigator t)
  (setq dashboard-icon-type 'all-the-icons) ; use `all-the-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile))

;; * Conf-desktop-mode for ini files
;; Enable conf-mode for .ini files
(add-to-list 'auto-mode-alist '("\\.ini\\'" . conf-desktop-mode))


;; * ripgrep
(require 'rg)
(rg-enable-default-bindings)

;; * Org-fragtog
;; Auto preview Latex
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; * Projectile
;; Optional: ag is nice alternative to using grep with Projectile
;; (use-package ag
;; :ensure t)

;; Optional: Enable vertico as the selection framework to use with Projectile
(use-package vertico
  :ensure t
  :init
  (vertico-mode +1))

;; Optional: which-key will show you options for partially completed keybindings
;; It's extremely useful for packages with many keybindings like Projectile.
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/vc/projects/"))
  (setq projectile-cleanup-known-projects nil)
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


;; * Org Mode Startup
(setq org-startup-folded t)
(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook 'follow-mode)
(setq org-babel-min-lines-for-block-output 1000)

;; * Org Crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;; * Org Capture
(setq org-directory "~/Downloads/NixOS/org")
(setq org-default-notes-file (concat org-directory "/tasks.org"))

;; * Org Alert
(require 'alert)
(use-package org-alert
  :ensure t)
(setq alert-default-style 'libnotify)
(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)

;; * All-the-icons
(when (display-graphic-p)
  (require 'all-the-icons))

;; * Neotree
(require 'neotree)
(global-set-key [f4] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-fixed-size nil)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

;; * Smartparens
(require 'smartparens-config)
(add-hook 'org-mode-hook #'smartparens-mode)
(sp-pair "$" "$")
(global-set-key (kbd "C-.") 'sp-rewrap-sexp)

;; * Electric Pair Mode
(electric-pair-mode t)
;; ** disable "<" pairing
(add-hook 'org-mode-hook (lambda ()
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))


;; * Org Agenda
(setq org-agenda-include-diary t)

;; * Treemacs
;; adjust the size in increments with 'shift-<' and 'shift->'
;; '?' to see all shortcuts. 'M-H' move UP rootdir, 'M-L' move DOWN rootdir
(setq treemacs-width-is-initially-locked nil)
(global-set-key [f9] 'treemacs)

;; * Python
(setq python-indent-guess-indent-offset nil)

;; * active Babel languages
;; (setq haskell-process-type 'ghci)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (haskell . t)
   (lua . t)
   (julia . t)
   (latex . t)
   (lisp . t)
   (nix . t)
   (rust . t)
   (spice . t)
   ))

;; * Spice
(setq spice-simulator "Ngspice"
      spice-waveform-viewer "ngplot")
;; ngplot is a new custom viewer defined in elisp which uses gnuplot


;; * Latex and preview pane
(latex-preview-pane-enable)
(setq org-preview-latex-default-process 'dvisvgm)
;; ** Scale Latex Preview Size
(defun my/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))
(defun my/text-scale--resize-fragment (ov)
  (overlay-put ov 'display
	       (cons 'image
		     (plist-put
		      (cdr (overlay-get ov 'display))
		      :scale (+ 1.0 (* 0.15 text-scale-mode-amount))
		      ;; :scale  (* +org-latex-preview-scale (expt text-scale-mode-step text-scale-mode-amount))
		      ))))
(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)
(advice-add 'org-fragtog--post-cmd :after #'my/text-scale-adjust-latex-previews)
;; ** Transparent Background
;; (eval-after-load 'org
;;   '(setf org-highlight-latex-and-related '(latex)))

;; * etc
;; (setq nb-notebook-directory "~/mountdir/Projects")
;; (org-babel-load-file "~/mountdir/emacs/scimax/scimax-notebook.org")
;; (setq org-image-actual-width 100)

(setq inferior-lisp-program "sbcl")
(setq org-src-block-faces 'nil)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; * epub reader
;; (setq nov-unzip-program (executable-find "bsdtar")
;; nov-unzip-args '("-xC" directory "-f" filename))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; ** epub reader
;; (use-package nov-xwidget
;;   :demand t
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))
;; (add-to-list 'load-path "/home/thinky/.emacs.d/elpa/nov-xwidget/")
;; (require 'nov-xwidget)

;; * PDF Tools
(pdf-tools-install)  ; Standard activation command
(pdf-loader-install) ; On demand loading, leads to faster startup time
(setq pdf-view-resize-factor 1.02)
(setq pdf-view-continuous nil)

(require 'saveplace-pdf-view)
(save-place-mode 1)

;; * Copy-and-Paste issue for org-src-block
(defun my-flush-lines ()
  "Calls flush-lines with a given regexp or ^$"
  (let ((regexp "^$"))
    (flush-lines regexp nil nil t)))

(defun myreplace ()
  "Beautify org src blk after copy and paste from PDF"
  (interactive)
  (org-babel-mark-block)
  (replace-regexp-in-region "'" "'")
  (replace-regexp-in-region """ "\"")
  (replace-regexp-in-region """ "\"")
  (replace-regexp-in-region "-" "-")
  (replace-regexp-in-region "\\#" "#")
  (query-replace-regexp "^[0-9]+\\. " "")
  (indent-for-tab-command)
  ;; (my-flush-lines)
  )

(global-set-key (kbd "<f4>") 'myreplace)

;; ;; * Direnv
;; (use-package direnv
;;   :ensure t
;;   :config
;;   (direnv-mode))

;; * Remove line after :results
;; (defun my-remove-line (_a _b)
;;   (save-excursion 
;;     (previous-line)
;;     (beginning-of-line)
;;     (when (looking-at-p "\n")
;;       (kill-line))))

;; (advice-add 'org-babel--insert-results-keyword :before #'my-remove-line)



;; * Company Mode
;; (add-hook 'after-init-hook 'global-company-mode)

;; * Magit
(keymap-global-set "C-x g" 'magit-status)
(keymap-global-set "C-x M-g" 'magit-dispatch)
(keymap-global-set "C-c M-g" 'magit-file-dispatch)

;; **************************************************

;; * Open ipynb
(require 'markdown-mode nil t)

(defun ipynb-to-markdown (file)
  (interactive "f")
  (let* ((data (with-temp-buffer
                 (insert-file-literally file)
                 (json-parse-string (buffer-string)
                                    :object-type 'alist
                                    :array-type 'list)))
         (metadata (alist-get 'metadata data))
         (kernelspec (alist-get 'kernelspec metadata))
         (language (alist-get 'language kernelspec)))
    (pop-to-buffer "ipynb-as-markdown")
    ;; (when (featurep 'markdown-mode)
    ;;   (markdown-mode))
    (dolist (c (alist-get 'cells data))
      (let* ((contents (alist-get 'source c))
             (outputs (alist-get 'outputs c)))
        (pcase (alist-get 'cell_type c)
          ("markdown"
           (when contents
             (mapcar #'insert contents)
             (insert "\n\n")))
          ("code"
           (when contents
             (insert "```")
             (insert language)
             (insert "\n")
             (mapcar #'insert contents)
             (insert "\n```\n\n")
             (dolist (x outputs)
               (when-let (text (alist-get 'text x))
                 (insert "```stdout\n")
                 (insert (mapconcat #'identity text ""))
                 (insert "\n```\n\n"))
               (when-let (data (alist-get 'data x))
                 (when-let (im64 (alist-get 'image/png data))
                   (let ((imdata (base64-decode-string im64)))
                     (insert-image (create-image imdata 'png t)))))
               (insert "\n\n")))))))))

;; * Convert ipynb to org
(setq code-cells-convert-ipynb-style '(
				       ("pandoc" "--to" "ipynb" "--from" "org")
				       ("pandoc" "--to" "org" "--from" "ipynb")
				       org-mode))


;; * Rust Mode
;; (require 'rust-mode)

;; * Replace function in scimax-ob.el
;; ;; * create/modify blocks

;; (defun scimax-ob-insert-src-block (&optional below)
;;   "Insert a src block above the current point.
;; With prefix arg BELOW, insert it below the current point.

;; If point is in a block, copy the header to the new block"
;;   (interactive "P")
;;   (if (org-in-src-block-p)
;;       (let* ((src (org-element-context))
;; 	     (start (org-element-property :begin src))
;; 	     (end (org-element-property :end src))
;; 	     (lang (org-element-property :language src))
;; 	     (switches (or (org-element-property :switches src) ""))
;; 	     (parameters (or (org-element-property :parameters src) ""))
;; 	     location)
;; 	(if below
;; 	    (progn
;; 	      (goto-char start)
;; 	      (setq location (org-babel-where-is-src-block-result nil nil))
;; 	      (if (not  location)
;; 		  (goto-char end)
;; 		(goto-char location)
;; 		(goto-char (org-element-property :end (org-element-context))))
;; 	      (insert (format "\n#+BEGIN_SRC %s %s %s

;; #+END_SRC\n" lang switches parameters))
;; 	      (forward-line -2))
;; 	  ;; after current block
;; 	  (goto-char (org-element-property :begin (org-element-context)))
;; 	  (insert (format "\n#+BEGIN_SRC %s %s %s

;; #+END_SRC\n" lang switches parameters))
;; 	  (forward-line -2)))

;;     ;; Not in a src block, just insert a block
;;     (beginning-of-line)
;;     (insert (format "\n#+BEGIN_SRC %s
;; #+END_SRC\n" (completing-read "Language: " (mapcar 'car org-babel-load-languages))))
;;     (forward-line -1)))
;;
;; ****************************************************


;; * Doom long file name error
;; (defun doom-make-hashed-auto-save-file-name-a (fn)
;;   "Compress the auto-save file name so paths don't get too long."
;;   (let ((buffer-file-name
;;          (if (or (null buffer-file-name)
;;                  (find-file-name-handler buffer-file-name 'make-auto-save-file-name))
;;              buffer-file-name
;;            (sha1 buffer-file-name))))
;;     (funcall fn)))
;; (advice-add #'make-auto-save-file-name :around #'doom-make-hashed-auto-save-file-name-a)

;; (defun doom-make-hashed-backup-file-name-a (fn file)
;;   "A few places use the backup file name so paths don't get too long."
;;   (let ((alist backup-directory-alist)
;;         backup-directory)
;;     (while alist
;;       (let ((elt (car alist)))
;;         (if (string-match (car elt) file)
;;             (setq backup-directory (cdr elt) alist nil)
;;           (setq alist (cdr alist)))))
;;     (let ((file (funcall fn file)))
;;       (if (or (null backup-directory)
;;               (not (file-name-absolute-p backup-directory)))
;;           file
;;         (expand-file-name (sha1 (file-name-nondirectory file))
;;                           (file-name-directory file))))))
;; (advice-add #'make-backup-file-name-1 :around #'doom-make-hashed-backup-file-name-a)

;; (require 'direnv)
;; (direnv-mode)

;; Org-nix-shell
;; (use-package org-nix-shell
;;   :straight '(org-nix-shell
;;               :type git
;;               :host github
;;               :repo "AntonHakansson/org-nix-shell")
;;   :hook (org-mode . org-nix-shell-mode))

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-mode t)

;;   ;; Set correct Python interpreter
;;   (setq pyvenv-post-activate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
;;   (setq pyvenv-post-deactivate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter "python3")))))

;; (require 'pyvenv)
;; (pyvenv-activate "~/Project/venv/")

;; Web-Mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (setq web-mode-engines-alist
;;       '(("django"  . "\\.html?\\'"))
;;       )

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-enable-auto-pairing t)

;; (setq org-emphasis-alist
;;       '(("*" (bold :foreground "Orange" ))
;; 	("/" italic)
;; 	("_" underline)
;; 	("=" (:background "maroon" :foreground "white"))
;; 	("~" (:background "deep sky blue" :foreground "MidnightBlue"))
;; 	("+" (:strike-through t))))

;; (setq mac-right-command-modifier 'hyper)
;; (setq mac-right-option-modifier 'super)

;; Latex Equation Zoom setting
;; sudo apt install texlive-latex-extra texlive-science -y
;; tlmgr init-usertree
;; sudo texhash
;; check /tmp/ log files for additional error.

;; * Org-Superstar
;; (require 'org-superstar)
;; (with-eval-after-load 'org-superstar
;;   (set-face-attribute 'org-superstar-item nil :height 1.2)
;;   (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
;;   (set-face-attribute 'org-superstar-leading nil :height 1.3))
;; ;; Set different bullets, with one getting a terminal fallback.
;; (setq org-superstar-headline-bullets-list
;;       '("◉" ("◆" ?◈) "○" "▷"))
;; ;; Stop cycling bullets to emphasize hierarchy of headlines.
;; (setq org-superstar-cycle-headline-bullets nil)
;; ;; Hide away leading stars on terminal.
;; (setq org-superstar-leading-fallback ?\s)
;; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; * Racket org babel
;; (use-package ob-racket
;;   :after org
;;   :config
;;   (add-hook 'ob-racket-pre-runtime-library-load-hook
;; 	    #'ob-racket-raco-make-runtime-library)
;;   :straight (ob-racket
;; 	     :type git :host github :repo "hasu/emacs-ob-racket"
;; 	     :files ("*.el" "*.rkt")))

;; * EAF Apps
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-mindmap)
;; (require 'eaf-image-viewer)
;; (require 'eaf-video-player)
;; (require 'eaf-terminal)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-manager)
;; (require 'eaf-jupyter)

;; ;; * Jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'jedi:ac-setup)

;; * Pyright
;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ; or lsp-deferred
