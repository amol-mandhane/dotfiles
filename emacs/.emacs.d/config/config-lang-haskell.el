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

(provide 'config-lang-haskell)
;;; config-lang-haskell.el ends here
