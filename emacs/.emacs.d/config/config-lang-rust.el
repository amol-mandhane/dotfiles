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
  (setq rustic-lsp-client 'eglot)
  (setq rustic-indent-offset 2)
  (add-hook 'rust-mode-hook #'(lambda () (setq-local whitespace-line-column 100)))
  (add-hook 'rustic-mode-hook #'(lambda () (setq-local whitespace-line-column 100)))
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode rustic-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  )

(provide 'config-lang-rust)
;;; config-lang-rust.el ends here
