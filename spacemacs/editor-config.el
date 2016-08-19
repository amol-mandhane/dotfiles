;;; Configuration related to the editing.

;; Require newline at end of file.
(setq require-final-newline t)

;; Evil additional settings
;; I make the typo of :W instead of :w very often.
(evil-ex-define-cmd "W" "w")

;; Google C-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(provide 'editor-config)
