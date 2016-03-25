(setq user-full-name "Amol Mandhane"
      user-mail-address "amol.mandhane@gmail.com")

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
                   badwolf-theme
                   company
                   evil
                   magit
                   org
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

; General settings
(setq tab-width 2
      indent-tabs-mode nil)
(setq make-backup-files nil)

; Default modes
(require 'evil)
(evil-mode 1)

; Theme
(if window-system
    (load-theme 'badwolf t)
  (load-theme 'wombat t))
