;;; config-lang-proto.el --- Protobuf configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)

(use-package protobuf-mode
  :straight t
  :mode "\\.proto\\'"
  :init
  (add-hook
   'protobuf-mode-hook
   (lambda ()
     (setq
      imenu-generic-expression
      '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(provide 'config-lang-proto)
;;; config-lang-proto.el ends here
