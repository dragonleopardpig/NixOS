(require 'flymake)

(defun my/flymake-toggle-diagnostics-buffer ()
  (interactive)
  ;; Check if we are in the diagnostics buffer.
  (if (string-search "*Flymake diagnostics" (buffer-name))
      (delete-window)
    (progn
      ;; Activate the Flymake diagnostics buffer.
      ;; and switch to it
      (flymake-show-buffer-diagnostics)
      (let ((name (flymake--diagnostics-buffer-name)))
        (if (get-buffer name)
            (switch-to-buffer-other-window name)
          (error "No Flymake diagnostics buffer found")
          )))))

(global-set-key [(f7)] #'my/flymake-toggle-diagnostics-buffer)

;; Additional bindings.
(global-set-key (kbd "C-c f b") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c f p") #'flymake-show-project-diagnostics)

;;;; `COMPANY'
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  ;; To prevent default down-casing.
  ;; https://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text
  (setq company-dabbrev-downcase nil)
  ;; 2023-01-13 From a Reddit post on mixed case issue.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-ignore-case nil))

;; Use `company' everywhere.
(add-hook 'after-init-hook 'global-company-mode)

(require 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-ts-mode-hook 'eglot-ensure)

(setq-default font-lock-maximum-decoration 3) ; Or a higher value if needed
