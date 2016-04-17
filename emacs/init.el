;;; emacs-config --- Configuration for Emacs editor

;;; Commentary:
;;  This file contains packages and their settings to be loaded in Emacs.  This
;;  is still a work-in-progress

;;; Code:

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
                           ag
                           ; airline-themes
                           auto-complete
                           auto-complete-c-headers
                           avy
                           badwolf-theme
                           beacon
                           company
                           diff-hl
                           evil
                           exec-path-from-shell
                           flx-ido
                           flycheck
                           flymake-google-cpplint
                           google-c-style
                           helm
                           helm-projectile
                           iedit
                           magit
                           neotree
                           nyan-mode
                           org
                           powerline
                           projectile
                           rainbow-delimiters
                           smartparens
                           spaceline
                           which-key
                           yasnippet
                           ; ycmd
                           ; company + langs
                           ) "Default packages to be installed at Emacs startup.")

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

;; Set up PATH
(exec-path-from-shell-initialize)

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
(add-hook 'window-setup-hook 'beacon-mode)

(require 'fill-column-indicator)
(setq fci-rule-column 81
      fci-rule-width 1
      fci-rule-color "dimgray")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))


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

(show-paren-mode +1)
(define-globalized-minor-mode
  global-rainbow-delimiters-mode
  rainbow-delimiters-mode (lambda () (rainbow-delimiters-mode 1)))
(global-rainbow-delimiters-mode +1)
(require 'smartparens-config)
(define-globalized-minor-mode
  global-smartparens-mode
  smartparens-mode (lambda () (smartparens-mode 1)))
(global-smartparens-mode +1)

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
(set-default-font "Inconsolata-12")

;; Shortcuts

(global-set-key (kbd "C-q") 'kill-this-buffer)

;; Machine specific config
(load "~/.emacs.machine.el")

;; Plugin settings
; (require 'powerline)
; (powerline-center-evil-theme)

;; Airline theme
; (require 'airline-themes)
; (load-theme 'airline-badwolf t)
; (setq powerline-default-separator 'arrow-fade)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq powerline-default-separator 'bar)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-buffer-position-off)
(nyan-mode +1)
(set-face-background 'powerline-active2 "grey30")
; (spaceline-toggle-evil-state-on)

;; Projectile
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-cache-file "~/.emacs.d/tmp/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.d/tmp/projectile-bookmarks.eld")

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
(define-key evil-insert-state-map (kbd "C-p") 'helm-projectile)

;; Ido + Flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-save-directory-list-file "~/.emacs.d/tmp/ido.last")

;; Avy
(require 'avy)
(define-key evil-normal-state-map (kbd ", ,") 'evil-avy-goto-word-or-subword-1)
(setq avy-background t)

;; diff-hl
(require 'diff-hl)
(define-globalized-minor-mode
  global-diff-hl-mode
  diff-hl-mode (lambda () (diff-hl-mode 1)))
(global-diff-hl-mode +1)
(diff-hl-margin-mode +1)
(diff-hl-flydiff-mode +1)
(setq diff-hl-margin-side 'right)

; FlyCheck
(global-flycheck-mode)

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(setq projectile-switch-project-action 'neotree-projectile-action)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; AutoComplete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-comphist-file (expand-file-name "~/.emacs.d/tmp/ac-comphist.dat"))
(ac-config-default)

; AutoComplete C headers
(defun ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0.0/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
)
(add-hook 'c++-mode-hook 'ac-c-headers-init)
(add-hook 'c-mode-hook 'ac-c-headers-init)

; google-cpplint
(defun flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load)
)
(add-hook 'c++-mode-hook 'flymake-google-init)
(add-hook 'c-mode-hook 'flymake-google-init)

; google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

; Semantic mode
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)

(defun semantic-init ()
  (semantic-mode t)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-hook 'semantic-init)
(add-hook 'c++-mode-hook 'semantic-init)

; YASnippet
(require 'yasnippet)
(yas-global-mode +1)

(provide 'init)
;;; init.el ends here
