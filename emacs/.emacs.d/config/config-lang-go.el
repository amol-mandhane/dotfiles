;;; config-lang-go.el --- Golang configuration       -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :config
  (defun go-mode-setup ()
    (add-hook 'before-save-hook #'gofmt-before-save)
    (setq-local tab-width 2)
    (setq gofmt-command "goimports")
    (go-guru-hl-identifier-mode +1))
  (add-hook 'go-mode-hook #'go-mode-setup))

(use-package company-go
  :straight t
  :after (company go-mode)
  :commands company-go
  :init (add-to-list 'company-backends 'company-go)
  :config (setq company-go-show-annotation t))

(use-package flycheck-gometalinter
  :straight t
  :after (flycheck go-mode)
  :hook (go-mode . flycheck-gometalinter-setup))

(use-package go-eldoc
  :straight t
  :hook (go-mode . go-eldoc-setup))

(use-package go-rename
  :if (executable-find "gorename")
  :straight t
  :bind (:map go-mode-map
              ("C-c r" . go-rename)))

(provide 'config-lang-go)
;;; config-lang-go.el ends here
