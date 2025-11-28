(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(setq lsp-pyright-auto-search-paths t) ; Automatically search for Python executables
