(setq user-full-name "Amol Mandhane"
      user-mail-address "amol.mandhane@gmail.com")

(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/third_party")

; Load clisp
(require 'cl)

; Package manager setup
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar default-packages '(
                   airline-themes
                   badwolf-theme
                   beacon
                   company
                   evil
                   magit
                   org
                   powerline
                   which-key
                   ; projectile
                   ; helm
                   ; ag
                   ; flycheck
                   ; ycmd
                   ; neotree / dired
                   ; surround
                   ; bracket-highlight
                   ; ace-jump / avy
                   ; rainbow-parath
                   ; company + langs
    ) "Default packages")

(defun check-all-packages-ok ()
  (loop for pkg in default-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (check-all-packages-ok)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg default-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

; Start-up options
(setq inhibit-splash-screen t
      initial-scratch-message nil)

; UI options
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'line-numbering)

(require 'beacon)
(beacon-mode +1)

(require 'fill-column-indicator)
(setq fci-rule-column 81
      fci-rule-width 1
      fci-rule-color "dimgray")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(require 'visual-indentation-mode)
(define-globalized-minor-mode
  global-indent-guides
  visual-indentation-mode (lambda () (visual-indentation-mode 1)))
(global-indent-guides +1)

(global-hl-line-mode +1)

;; Key binding suggestions
(require 'which-key)
(which-key-mode +1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

; General settings
(require 'indentation)

(setq make-backup-files nil)

(require 'whitespace)
(setq whitespace-line-column 101)
(setq whitespace-style (quote (face trailing tabs lines-tail newline)))
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-whitespace-mode t)

;; New line at the end
(setq require-final-newline t)

;; Auto reload
(global-auto-revert-mode t)

;; History
(setq savehist-file "~/.emacs.d/tmp/history")
(savehist-mode +1)

;; Centering search results
(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

; Default modes
(require 'evil)
(evil-mode +1)

(require 'org)

; Theme
(if window-system
    (load-theme 'badwolf t)
  (load-theme 'wombat t))
(setq ns-use-srgb-colorspace nil)
(set-default-font "Inconsolata-dz for Powerline 14")

;; Shortcuts

(global-set-key (kbd "C-q") 'kill-this-buffer)

;; Machine specific config
(load "~/.emacs.machine.el")

;; Plugin settings
(require 'powerline)
(powerline-center-evil-theme)
(require 'airline-themes)
(load-theme 'airline-badwolf t)
(setq powerline-default-separator 'arrow-fade)
