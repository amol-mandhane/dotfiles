;;; config.el --- Main configuration file            -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'config-straight)
(require 'config-keybinding)
(require 'config-ui)
(require 'config-editor)
(require 'config-search)
(require 'config-vc)
(require 'config-code-assist)

(require 'config-lang-lisp)
(require 'config-lang-c)
(require 'config-lang-go)
(require 'config-lang-haskell)
(require 'config-lang-proto)
(require 'config-lang-shell)
(require 'config-lang-python)
(require 'config-lang-text)

;; TODO: Set up terminal, elfeed.

(require 'config-machine)

(provide 'config)
;;; config.el ends here
