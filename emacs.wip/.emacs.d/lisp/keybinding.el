;;; keybinding.el --- Helper library for creating keybindings.

;;; Commentary:

;; This is a wrapper library around `bind-key', `global-set-key' and `define-key' methods.
;;
;; Usage:
;;
;;   (global-key "C-v" describe-variable)
;;
;;   (global-keys ("C-v" . describe-variable) ("C-f" . describe-function))
;;
;;   (mode-key emacs-lisp-mode-map "C-v" describe-variable)
;;
;;   (mode-keys emacs-lisp-mode-map ("C-v" . describe-variable) ("C-f" . describe-function))
;;
;;   ;; Binds key "M-m hv" to `describe-variable'. Prefix can be changed by changing
;;   ;; `+keybinding/mnemonic-prefix+'
;;   (prefixed-key "hv" describe-variable)
;;
;;   (prefixed-keys ("hv" . describe-variable) ("hf" . describe-function))
;;
;;   (prefixed-mode-key emacs-lisp-mode-map "hv" describe-variable)
;;
;;   (prefixed-mode-keys emacs-lisp-mode-map ("hv" . describe-variable) ("hf" . describe-function))
;;
;;   (rename-mnemonic-key-prefix "h" "Help")
;;
;;   (rename-key-prefix "C-c" "Ctrl-C prefix")

;;; Code:

(require 'cl-lib)

(defconst +keybinding/mnemonic-prefix+ "M-m" "Prefix of the mnemonic keybindings.")

(defalias 'global-key 'bind-key*
  "Define global keybinding which overrides all minor mode keybindings. Alias for bind-key*.")

(defalias 'global-keys 'bind-keys*
  "Define multiple global keybindings which override all minor mode keybinding. Alias for bind-keys*.")

(defmacro mode-key (keymap key-string function)
  "Define keybinding for specific keymap.

  KEYMAP: Keymap to add the binding to.
  KEY-STRING: `kbd' style string representation of keybinding.
  FUNCTION: Function to associate the keybinding to."

  `(define-key ,keymap (kbd ,key-string) (quote ,function)))

(defmacro mode-keys (keymap &rest keys)
  "Define keybindings for specific keymap.

  KEYMAP: Keymap to add the binding t
  KEYS: conses of (`kbd' style keybinding string . function) for keybinding definition."
  `(progn ,@(cl-loop
	     for keybd in keys
	     collect `(mode-key ,keymap ,(car keybd) ,(cdr keybd)))))

(defmacro prefixed-key (key-string function)
  "Define key with mnemonic prefix.

  KEY-STRING: `kbd' style keybinding string.
  FUNCTION: Function to bind the key to."
  `(bind-keys ((concat +keybinding/mnemonic-prefix+ " " ,key-string) . ,function)))

(defmacro prefixed-keys (&rest keys)
  "Define keys with mnemonic prefix.

  KEYS: conses of (`kbd' style keybinding string . function) for keybinding definition."
  `(progn ,@(cl-loop
	     for keybd in keys
	     collect `(prefixed-key ,(car keybd) ,(cdr keybd)))))

(defmacro prefixed-mode-key (keymap key-string function)
  "Define key with mnemonic prefix within the given keymap.

  KEYMAP: Keymap to add the binding to.
  KEY-STRING: `kbd' style keybinding string.
  FUNCTION: Function to bind the key to."
  `(bind-keys :map keymap ((concat +keybinding/mnemonic-prefix+ " " ,key-string) . ,function)))

(defmacro prefixed-mode-keys (keymap &rest keys)
  "Define keys with mnemonic prefox within the given keymap.

  KEYMAP: Keymap to add the binding to.
  KEYS: conses of (`kbd' style keybinding string . function) for keybinding definition."
  `(progn ,@(cl-loop
	     for keybd in keys
	     collect `(prefixed-mode-key ,keymap ,(car keybd) ,(cdr keybd)))))

(defun rename-mnemonic-key-prefix (key-string name)
  "Add label to the mnemonic keybinding prefix to be displayed in `which-key'.

  KEY-STRING: `kbd' style keybinding string.
  NAME: String label."
  (which-key-add-key-based-replacements (concat +keybinding/mnemonic-prefix+ " " key-string) name))

(defun rename-key-prefix (key-string name)
  "Add label to the keybinding prefix to be displayed in `which-key'.

  KEY-STRING: `kbd' style keybinding string.
  NAME: String label."
  (which-key-add-key-based-replacements (concat +keybinding/mnemonic-prefix+ " " key-string) name))

(provide 'keybinding)
;;; keybinding.el ends here
