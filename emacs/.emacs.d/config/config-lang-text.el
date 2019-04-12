;;; config-lang-text.el --- Text mode configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package tex
  :disabled
  :straight auctex
  :pin gnu
  :config
  (setq TeX-parse-self t)  ;; Enable parse on load.
  (setq TeX-auto-save t)  ;; Enable parse on save.
  (setq TeX-PDF-mode t))

(use-package company-auctex
  :disabled
  :straight t
  :after (company tex)
  :hook (after-init . company-auctex-init))

(use-package org
  :straight t
  :pin gnu
  :bind (:map org-mode-map
              ("C-<up>" . org-move-subtree-up)
              ("C-<down>" . org-move-subtree-down))
  :config
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t))

(use-package org-bullets
  :straight t
  :after (org)
  :hook (org-mode . org-bullets-mode))

(use-package org-indent
  :hook (org-mode . org-indent-mode))

(provide 'config-lang-text)
;;; config-lang-text.el ends here
