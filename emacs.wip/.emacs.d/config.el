
(setq user-full-name "Amol Mandhane"
      user-mail-address "amol.mandhane@gmail.com")

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(global-unset-key (kbd "C-."))
(require 'keybinding)

(use-package smex
  :ensure t)
(use-package ido-ubiquitous
  :ensure t)
(use-package ido-vertical-mode
  :ensure t)
(use-package flx
  :ensure t
  :after ido
  :config (flx-ido-mode +1))

(use-package ido
  :ensure t
  :after smex
  :after ido-ubiquitous
  :after ido-vertical-mode
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-use-virtual-buffers t)
    (setq ido-enable-regex t)

    (add-hook
      'ido-setup-hook
      (lambda () (mode-keys
                   ido-completion-map
                   ("<tab>" . ido-exit-minibuffer)
                   ("<return>" . ido-exit-minibuffer))))

    (ido-mode +1)
    (ido-everywhere +1)
    (ido-ubiquitous-mode +1)
    (ido-vertical-mode +1)

    (require 'smex)
    (smex-initialize)))

(use-package ivy
  :ensure t
  :after flx
  :diminish ivy-mode
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "[%d / %d] ")
    (ivy-mode +1)
    (setq ivy-re-builders-alist
          '((counsel-M-x . ivy--regex-fuzzy)
            (counsel-find-file . ivy--regex-fuzzy)
            (t . ivy--regex-plus)))
    (global-keys
      ("M-x" . counsel-M-x)
      ("C-c M-x" . execute-extended-command)
      ("C-x C-f" . counsel-find-file))))

(setq delete-old-versions -1 )          ; delete excess backup versions silently
(setq version-control t )               ; use version control
(setq vc-make-backup-files t )          ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )                                   ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )        ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )      ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )   ; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)    ; sentence SHOULD end with only a point.
(setq default-fill-column 80)           ; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup

(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-linum-mode t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(global-hl-line-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(setq ns-use-srgb-colorspace nil)

(setq require-final-newline t)

(set-default-font "Inconsolata-16")

(setq cursor-type 'bar)
(blink-cursor-mode 0)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode t)
  :config
    (setq which-key-sort-order 'which-key-key-order-alpha
      which-key-side-window-max-width 0.33
      which-key-idle-delay 0.05))

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t))

(use-package company
  :ensure t
  :diminish (company-mode . " Ξ")
  :init (global-company-mode t))

; (use-package spaceline
;   :ensure t
;   :config
;     (require 'spaceline-config)
;     (spaceline-emacs-theme))

(use-package smart-mode-line
  :ensure t
  :config
    (sml/setup))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . " !")
  :config
  (progn
    (global-flycheck-mode t)
    (rename-key-prefix "e" "Errors")
    (prefixed-keys
     ("en" . flycheck-next-error)
     ("ep" . flycheck-previous-error))))

; (electric-pair-mode +1)

(defmacro def-pair (pair)
  "Creates function sp/wrap-with-<PAIR>."
  `(progn (defun ,(read (concat "sp/wrap-with-"
                                (prin1-to-string (car pair))
                                "s")) (&optional arg)
            (interactive "p")
            (sp-wrap-with-pair ,(cdr pair)))))

(def-pair (paren . "("))
(def-pair (bracket . "["))
(def-pair (brace . "{"))
(def-pair (single-quote . "'"))
(def-pair (double-quote . "\""))
(def-pair (back-quote . "`"))

(use-package smartparens-config
  :ensure smartparens
  :diminish (smartparens-mode . " ✓")
  :config
  (smartparens-global-mode +1)
  (mode-keys smartparens-mode-map
             ;; Navigation
             ("C-M-a" . sp-beginning-of-sexp)
             ("C-M-e" . sp-end-of-sexp)
             ("C-M-f" . sp-forward-sexp)
             ("C-M-b" . sp-backward-sexp)

             ;; Traversal
             ("C-<down>" . sp-down-sexp)
             ("C-<up>" . sp-up-sexp)
             ("M-<down>" . sp-backward-down-sexp)
             ("M-<up>" . sp-backward-up-sexp)
             ("C-M-n" . sp-next-sexp)
             ("C-M-p" . sp-previous-sexp)
             ("C-S-f" . sp-forward-symbol)
             ("C-S-b" . sp-backward-symbol)

             ;; AST re-arrange.
             ;; ("C-)" . sp-forward-slurp-sexp)
             ("C-)" . sp-slurp-hybrid-sexp)
             ("C-}" . sp-forward-barf-sexp)
             ("C-(" . sp-backward-slurp-sexp)
             ("C-{" . sp-backward-barf-sexp)

             ;; Killing
             ("C-M-k" . sp-kill-sexp)
             ("C-k" . sp-kill-hybrid-sexp)
             ("M-k" . sp-backward-kill-sexp)

             ;; Unknown
             ;; ("C-M-t" . sp-transpose-sexp)
             ;; ("C-M-w" . sp-copy-sexp)
             ;; ("C-M-d" . delete-sexp)
             ;; ("M-<backspace>" . backward-kill-word)
             ;; ("C-<backspace>" . sp-backward-kill-word)
             ;; ([remap sp-backward-kill-word] . backward-kill-word)
             ;; ("M-[" . sp-backward-unwrap-sexp)
             ;; ("M-]" . sp-unwrap-sexp)
             ;; ("C-x C-t" . sp-transpose-hybrid-sexp)

             ;; Wrap
             ;; ("C-c C-w (" . sp/wrap-with-parens)
             ;; ("C-c C-w [" . sp/wrap-with-brackets)
             ;; ("C-c C-w {" . sp/wrap-with-braces)
             ;; ("C-c C-w '" . sp/wrap-with-single-quotes)
             ;; ("C-c C-w \"" . sp/wrap-with-double-quotes)
             ;; ("C-c C-w `" . sp/wrap-with-back-quotes)
             ))

(electric-indent-mode +1)

(use-package avy
  :ensure t
  :config
    (prefixed-key "," avy-goto-word-1))

(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/tmp/history")

(savehist-mode +1)

(require 'recentf)
(recentf-mode +1)
(setq recentf-max-menu-items 25)

;; Save recent files every few minutes.
(run-at-time nil (* 5 60) 'recentf-save-list)

(use-package magit
  :ensure t
  :config
  (progn
    (prefixed-key "gs" magit-status)))

(use-package monky
  :ensure t
  :config
  (progn
    (prefixed-key "gh" monky-status)))
