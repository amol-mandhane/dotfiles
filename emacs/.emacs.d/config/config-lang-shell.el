;;; config-lang-shell.el --- Shell script configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)

(defconst +zsh-filename-patterns+
  '("\\.zsh\\'"
    "zlogin\\'"
    "zlogout\\'"
    "zpreztorc\\'"
    "zprofile\\'"
    "zshenv\\'"
    "zshrc\\'")
  "Filename patterns for Zsh script files.")

(use-package sh-script
  :commands (sh-set-shell)
  :init
  (dolist (pattern +zsh-filename-patterns+)
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
  (add-hook
   'sh-mode-hook
   (lambda ()
     (when
         (and
          buffer-file-name
          (cl-mapcar
           (lambda (pat) (string-match-p pat buffer-file-name))
           +zsh-filename-patterns+))
       (sh-set-shell "zsh")))))

(use-package company-shell
  :straight t
  :commands company-shell
  :after company
  :init
  (add-hook
   'sh-mode-hook
   (lambda ()
     (add-to-list
      (make-local-variable 'company-backends)
      'company-shell))))

(use-package insert-shebang
  :straight t
  :config
  (remove-hook 'find-file-hook 'insert-shebang))

(provide 'config-lang-shell)
;;; config-lang-shell.el ends here
