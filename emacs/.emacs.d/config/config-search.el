;;; config-search.el --- Search/Replace configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(require 'straight)
(require 'use-package)

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

(use-package swiper-helm
  :straight t
  :chords (("??" . swiper-helm))
  :prefixed-bind ("ss" . swiper-helm))

(use-package anzu
  :straight t
  :hook (after-init . global-anzu-mode)
  :prefixed-bind (("srr" . anzu-query-replace-regexp)
                  ("sr." . anzu-query-replace-at-cursor-thing)))

(use-package iedit
  :straight t
  :bind (("C-'" . iedit-mode)))

(use-package avy
  :straight t
  :chords (("jj" . avy-goto-word-1)))

(use-package ag
  :straight t)
(use-package wgrep
  :straight t)
(use-package wgrep-ag
  :straight t)

(provide 'config-search)
;;; config-search.el ends here
