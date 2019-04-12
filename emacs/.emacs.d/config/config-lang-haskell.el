;;; config-lang-haskell.el --- Haskell configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-c d" . haskell-debug)
              ("C-c i" . haskell-interactive-switch)
              ("C-c t" . haskell-process-do-type)
              ("C-c h" . haskell-process-do-info)
              ("C-c fi" . haskell-add-import)
              ("C-c ff" . haskell-mode-stylish-buffer))
  :config
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indent))

(use-package hindent
  :straight t
  :if (executable-find "hindent")
  :hook (haskell-mode . hindent-mode)
  :config
  ;; reformat the buffer using hindent on save
  (setq hindent-reformat-buffer-on-save t))

(use-package intero
  :straight t
  :after (haskell-mode company flycheck)
  :hook (haskell-mode . intero-mode)
  :config
  (add-to-list 'flycheck-ghc-search-path (expand-file-name "~/.xmonad/lib")))

(provide 'config-lang-haskell)
;;; config-lang-haskell.el ends here
