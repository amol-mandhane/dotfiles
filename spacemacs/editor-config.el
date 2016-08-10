;;; Configuration related to the editing.

;; Require newline at end of file.
(setq require-final-newline t)

;; Google C-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(provide 'editor-config)
