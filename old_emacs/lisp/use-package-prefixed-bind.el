;;; use-package-prefixed-bind.el --- :prefixed-bind keyword for use-package  -*- lexical-binding: t; -*-

;;; Commentary:

;; This library adds :prefixed-bind keyword to `use-package' command.  Prefix
;; can be adjusted using variable +keybinding/mnemonic-prefix+.
;;
;; Usage:
;; (use-package package-name
;;   :prefixed-bind (("kk" . function)
;;                   :map key-map
;;                   ("dd" . function)))

;;; Code:
(require 'use-package)

(global-unset-key (kbd "M-m"))

(defconst +keybinding/mnemonic-prefix+ "M-m"
  "Prefix of the mnemonic keybindings.")

(defun use-package-normalize/:prefixed-bind (name keyword args)
  "Normalize :prefixed-bind arguments for use-package NAME with label KEYWORD.

Add prefix to the keys in ARGS."
  (let ((clean-args (use-package-normalize-binder name keyword args)))
    (mapcar
     (lambda (x)
       (if (and (consp x)
                (stringp (car x)))
           `(,(concat +keybinding/mnemonic-prefix+ " " (car x)) . ,(cdr x))
         x))
     clean-args)))

(defalias 'use-package-handler/:prefixed-bind #'use-package-handler/:bind)

(add-to-list 'use-package-keywords :prefixed-bind t)

(provide 'use-package-prefixed-bind)
;;; use-package-prefixed-bind.el ends here
