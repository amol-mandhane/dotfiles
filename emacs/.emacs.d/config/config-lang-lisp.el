;;; config-lang-lisp.el --- Config for Lisp-family languages  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)
(require 'crux)

(defvar lisp-family-mode-hook nil
  "Hook for Lisp family major modes.")

(use-package elisp-mode
  :demand
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . elisp-visit-ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)
              ("C-c C-r" . eval-region))
  :hook (emacs-lisp-mode . emacs-lisp-mode-setup)
  :init
  (progn
    (defun elisp-visit-ielm ()
      "Switch to default `ielm' buffer.
       Start `ielm' if it's not already running."
      (interactive)
      (crux-start-or-switch-to 'ielm "*ielm*"))

    (defun elisp-recompile-elc-on-save ()
      "Recompile when saving an elisp file."
      (add-hook
       'after-save-hook
       (lambda ()
         (when (file-exists-p (byte-compile-dest-file buffer-file-name))
           (emacs-lisp-byte-compile)))
       nil
       :local))

    (defun emacs-lisp-mode-setup ()
      "Setup for emacs-lisp mode."
      (elisp-recompile-elc-on-save)
      (smartparens-strict-mode +1)
      (setq mode-name "ELisp"))))

(use-package litable
  :straight t
  :bind (:map emacs-lisp-mode-map
              ("C-c l" . litable-mode)
              :map lisp-interaction-mode-map
              ("C-c l" . litable-mode)
              :map litable-mode-map
              ("C-c p" . litable-accept-as-pure))

  :config
  (setq litable-list-file "~/.emacs.d/litable-lists.el"))

(use-package pp
  :straight t)

(add-hook
 'lisp-interaction-mode-hook (lambda () (run-hooks 'emacs-lisp-mode-hook)))

(use-package redshank
  :straight t
  :hook (lisp-family-mode . redshank-mode))

(use-package macrostep
  :straight t
  :bind (:map emacs-lisp-mode-map
              ("C-c m" . macrostep-mode)
              :map lisp-mode-map
              ("C-c m" . macrostep-mode)))

(provide 'config-lang-lisp)
;;; config-lang-lisp.el ends here
