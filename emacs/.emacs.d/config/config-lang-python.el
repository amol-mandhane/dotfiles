;;; config-lang-python.el --- Python configuration   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)
(require 'helper-lib)

(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq python-shell-interpreter "/usr/local/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

(use-package elpy
  :straight t
  :unless (restricted-config-p)
  :hook (after-init . elpy-enable)
  :config
  (setq elpy-eldoc-show-current-function nil))

(use-package py-yapf
  :straight t
  :unless (restricted-config-p)
  :commands py-yapf)

(provide 'config-lang-python)
;;; config-lang-python.el ends here
