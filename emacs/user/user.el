;; Disable some defaults
(disable-theme 'smart-mode-line-light)
(google-this-mode -1)

;; Set Faces, etc...
(set-face-attribute 'default nil :height 130)
(setq org-startup-folded t)
(global-visual-line-mode t)
(global-display-fill-column-indicator-mode t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  (lambda() (set-fill-column 80)))
(column-number-mode)

;; GUI Interface
;; (delete-selection-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-set-navigator t)

;; Org Crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)

;; Org Capture
(setq org-directory "~/mountdir/org")
(setq org-default-notes-file (concat org-directory "/tasks.org"))

;; Org Alert
(require 'alert)
(use-package org-alert
  :ensure t)
(setq alert-default-style 'libnotify)
(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)

;; All-the-icons
(when (display-graphic-p)
  (require 'all-the-icons))

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-fixed-size nil)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)


;; Electric Pair Mode
(electric-pair-mode t)

;; Org-Superstar
(require 'org-superstar)
(with-eval-after-load 'org-superstar
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3))
;; Set different bullets, with one getting a terminal fallback.
(setq org-superstar-headline-bullets-list
      '("◉" ("◆" ?◈) "○" "▷"))
;; Stop cycling bullets to emphasize hierarchy of headlines.
(setq org-superstar-cycle-headline-bullets nil)
;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Text Step Scale
(setq text-scale-mode-step 1.1)

;; Org Agenda
;; (setq org-agenda-include-diary t)

;; treemacs
;; adjust the size in increments with 'shift-<' and 'shift->'
;; '?' to see all shortcuts. 'M-H' move UP rootdir, 'M-L' move DOWN rootdir
(setq treemacs-width-is-initially-locked nil)
(global-set-key [f9] 'treemacs)



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




;; Python
(setq python-indent-guess-indent-offset nil)

;; active Babel languages
;; (setq haskell-process-type 'ghci)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (haskell . t)
   ;; (julia . t)
   (latex . t)
   (lisp . t)
   (nix . t)
   )
 )

;; (setq mac-right-command-modifier 'hyper)
;; (setq mac-right-option-modifier 'super)

;; Latex Equation Zoom setting
;; sudo apt install texlive-latex-extra texlive-science -y
;; tlmgr init-usertree
;; sudo texhash
;; check /tmp/ log files for additional error.

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.25))
(setq org-preview-latex-default-process 'dvisvgm)

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
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

;; (setq nb-notebook-directory "~/mountdir/Projects")
;; (org-babel-load-file "~/mountdir/emacs/scimax/scimax-notebook.org")
(setq org-image-actual-width 100)

;; Doom long file name error
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

;; Latex preview pane
(latex-preview-pane-enable)


;; (require 'direnv)
;; (direnv-mode)

;; Org-nix-shell
;; (use-package org-nix-shell
;;   :straight '(org-nix-shell
;;               :type git
;;               :host github
;;               :repo "AntonHakansson/org-nix-shell")
;;   :hook (org-mode . org-nix-shell-mode))

(setq inferior-lisp-program "sbcl")
(setq org-src-block-faces 'nil)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
