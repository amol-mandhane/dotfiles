;;; config-lang-python.el --- Python configuration   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)
(require 'helper-lib)

(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :after (company eglot)
  :hook (python-mode . eglot-ensure)
  :init
  (setq python-shell-interpreter "/usr/local/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode) .("ruff" "server")))
  (add-to-list 'eglot-server-programs
               '((python-mode) .("pyright-langserver" "--stdio"))))

(provide 'config-lang-python)
;;; config-lang-python.el ends here
