;;; heaven-and-hell-custom.el --- Custom org-mode styling for theme switching -*- lexical-binding: t; -*-

;; Custom function to apply org-mode faces based on theme type
(defun my/heaven-and-hell-apply-org-faces ()
  "Apply custom org-mode faces when switching between light and dark themes."
  (if (eq heaven-and-hell-theme-type 'dark)
      ;; Dark theme org customizations
      (progn
        (setq org-src-block-faces
              '(("emacs-lisp" (:background "LightCyan1" :extend t))
                ("sh" (:background "#2C001E" :extend t))
                ("jupyter-python" (:background "#001f26" :extend t))
                ("ipython" (:background "HotPink4" :extend t))
                ("python" (:background "#1d2100" :extend t))
                ("sqlite" (:background "#a24224" :extend t))
                ("haskell" (:background "#4200a2" :extend t))
                ("nix" (:background "maroon" :extend t))
                ("lisp" (:background "#232627" :extend t))))
        (custom-set-faces
         '(org-block-begin-line
           ((t (:underline t
                :overline nil
                :foreground "pink"
                :background nil
                :italic t
                :bold t
                :extend t))))
         '(org-block-end-line
           ((t (:underline nil
                :overline t
                :foreground "pink"
                :background nil
                :italic t
                :bold t
                :extend t))))
         '(org-level-1 ((t (:foreground "salmon" :italic t :bold t)))))
        (set-background-color "#232627"))

    ;; Light theme org customizations
    (progn
      (setq org-src-block-faces
            '(("emacs-lisp" (:background "LightCyan1" :extend t))
              ("sh" (:background "gray90" :extend t))
              ("jupyter-python" (:background "snow" :extend t))
              ("ipython" (:background "thistle1" :extend t))
              ("python" (:background "DarkSeaGreen1" :extend t))
              ("sqlite" (:background "#a24224" :extend t))
              ("haskell" (:background "#4200a2" :extend t))
              ("nix" (:background "light yellow" :extend t))
              ("lisp" (:background "honeydew" :extend t))))
      (custom-set-faces
       '(org-block-begin-line
         ((t (:underline t
              :overline nil
              :foreground "orange red"
              :background "white"
              :italic t
              :bold t
              :extend t))))
       '(org-block-end-line
         ((t (:underline nil
              :overline t
              :foreground "orange red"
              :background "white"
              :italic t
              :bold t
              :extend t))))
       '(org-level-1
         ((t (:foreground "red3"
              :italic t))))
       '(org-list-dt
         ((t (:foreground "#008080"
              :bold t)))))
      (set-background-color "white")))

  ;; Refresh org-mode buffers if any are open
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (org-mode-restart)
        (when (outline-on-heading-p)
          (outline-show-children)
          (outline-show-entry))))))

;; Add advice to heaven-and-hell-clean-load-themes to apply org customizations
(advice-add 'heaven-and-hell-clean-load-themes :after
            (lambda (&rest _args)
              (my/heaven-and-hell-apply-org-faces)))

(provide 'heaven-and-hell-custom)
;;; heaven-and-hell-custom.el ends here
