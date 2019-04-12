;;; config-lang-c.el --- C/C++ configuration         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)
(require 'helper-lib)

(use-package google-c-style
  :straight t
  :hook (c-mode-common . google-set-c-style))

(use-package cc-mode
  :init
  (add-hook
   'java-mode-hook
   (lambda () (setq fill-column 100
                    whitespace-line-column 100))))

(use-package irony
  :unless (restricted-config-p)
  :straight t
  :hook (c++-mode . irony-mode)
  :hook (c-mode . irony-mode)
  :hook (objc-mode . irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :unless (restricted-config-p)
  :straight t
  :commands company-irony
  :after (company irony)
  :init
  (add-to-list 'company-backends 'company-irony)
  (add-hook
   'c++-mode-hook
   (lambda ()
     (setq-local company-backends (delete 'company-clang company-backends)))))

(use-package company-irony-c-headers
  :unless (restricted-config-p)
  :straight t
  :commands company-irony-c-headers
  :after (company irony)
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package flycheck-irony
  :unless (restricted-config-p)
  :straight t
  :after (flycheck irony)
  :hook (c-mode-common . flycheck-irony-setup))

(use-package irony-eldoc
  :unless (restricted-config-p)
  :straight t
  :after (irony)
  :hook (irony-mode . irony-eldoc))

;; Maybe RTags?

(provide 'config-lang-c)
;;; config-lang-c.el ends here
