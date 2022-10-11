;;; config-lang-lean.el --- Lean3 configuration         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)
(require 'helper-lib)

(use-package lean-mode
  :straight t
  :mode ("\\.lean\\'" . lean-mode)
  :mode ("\\.hlean\\'" . lean-mode))

(use-package company-lean
  :straight t
  :after lean-mode)

(use-package helm-lean
  :straight t
  :after lean-mode)

(provide 'config-lang-lean)
;;; config-lang-lean.el ends here
