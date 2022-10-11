;;; config-lang-rust.el --- Rush configuration       -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)

(use-package rustic
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :after (company eglot)
  :config
  (setq rustic-lsp-client 'eglot))

(provide 'config-lang-rust)
;;; config-lang-rust.el ends here
