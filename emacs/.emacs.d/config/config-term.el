;;; config-term.el --- Configuration of terminal in emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Terminal configuration using vterm.

;;; Code:

(require 'straight)
(require 'use-package)

(use-package vterm
  :straight t)

(use-package multi-vterm
  :straight (multi-vterm
             :type git
             :host github
             :repo "suonlight/multi-vterm")
  :prefixed-bind (("tn" . multi-vterm)
                  ("tp" . multi-vterm-projectile)))

(provide 'config-term)
;;; config-term.el ends here
