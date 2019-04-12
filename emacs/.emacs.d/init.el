;;; init.el --- init file.                           -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq load-prefer-newer t)

(let
    ((enable-local-eval t)
     (inhibit-message t)
     (ad-redefinition-action 'accept)
     (file-name-handler-alist nil)
     (gc-cons-threshold 640000000))
  (add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list
   'load-path
   (expand-file-name "straight/repos/straight.el" user-emacs-directory))
  (require 'config))

(add-hook 'after-init-hook (lambda () (server-start)))
(provide 'init)
;;; init.el ends here
