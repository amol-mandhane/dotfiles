;;; config-keybinding.el --- Common keybinding config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)

(setq +keybinding/mnemonic-prefix+ "M-m")

(use-package use-package-chords
  :straight t
  :config
  (key-chord-mode +1)
  (setq key-chord-two-keys-delay 0.05))

(use-package use-package-prefixed-bind
  :demand t)

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.5)

  (defun rename-mnemonic-key-prefix (key-string name)
    (which-key-add-key-based-replacements
     (concat +keybinding/mnemonic-prefix+ " " key-string) name))

  (rename-mnemonic-key-prefix "!" "Terminal")
  (rename-mnemonic-key-prefix "b" "Buffers")
  (rename-mnemonic-key-prefix "c" "Compilation")
  (rename-mnemonic-key-prefix "e" "Errors")
  (rename-mnemonic-key-prefix "f" "Files")
  (rename-mnemonic-key-prefix "g" "VCS")
  (rename-mnemonic-key-prefix "p" "Projects")
  (rename-mnemonic-key-prefix "r" "Ring/Register")
  (rename-mnemonic-key-prefix "s" "Search/Replace")
  (rename-mnemonic-key-prefix "t" "Tags")
  (rename-mnemonic-key-prefix "w" "Windows"))

(use-package hydra
  :straight t
  :demand t)

(provide 'config-keybinding)
;;; config-keybinding.el ends here
