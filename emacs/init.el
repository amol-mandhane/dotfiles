(setq visible-bell t)

(require 'package)
(require 'json)

(add-to-list 'load-path "~/.emacs.d/load/")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Interface config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color scheme
(require 'color-theme)
;;(setq color-theme-is-global t)
(color-theme-monokai)

;; Disable splash
(setq inhibit-startup-message t)

;; Disable toolbars
(tool-bar-mode -1)
;; (menu-bar-mode -1)

;; Scrollbar
(require 'yascroll)
(global-yascroll-bar-mode 1)
(scroll-bar-mode -1)
(setq scroll-margin 2)
(setq next-line-add-newlines nil)

;; Line numbers and highlighting
(global-linum-mode 1)
(setq linum-format " %d  ")
(require 'crosshairs)
(crosshairs-mode)
(setq col-highlight-vline-face-flag 1)
;;(require 'vline)
;;(global-hl-line-mode 1)
;;(vline-global-mode 1)
(require 'column-marker)



;; Font
(set-default-font "Inconsolata-11")

;; Whitespace
(require 'whitespace)
(require 'whitespace-cleanup-mode)
(require 'blank-mode)
(setq whitespace-style '(empty tabs lines-tail trailing))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq blank-chars '(tabs trailing newline indentation empty))
;(global-blank-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functionality config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable backup
(setq backup-inhibited t)

;; Disable autosave
(setq auto-save-default nil)

; Undo levels
(setq undo-limit 3600)

;;;; Plugin config
(require 'evil)
(evil-mode 1)

(require 'powerline)
(powerline-default-theme)
;; (display-time-mode t)


;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:background "#141411")))))
