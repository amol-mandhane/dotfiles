(setq user-full-name "Amol Mandhane"
      user-mail-address "amol.mandhane@gmail.com")

(setq ad-redefinition-action 'accept)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";;; Lisp Interaction Mode\n")
(setq initial-major-mode 'lisp-interaction-mode)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq visible-bell t)

(setq ns-use-srgb-colorspace nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)
(column-number-mode -1)
(size-indication-mode -1)

(unless (frame-parameter nil 'fullscreen)
  (if
      (eq system-type 'darwin)
      (toggle-frame-fullscreen)
    (toggle-frame-maximized)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

(require 'package)
;; Package initialized in init.el
;; (package-initialize)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(add-hook 'after-init-hook #'(lambda () (server-start)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode t)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05))

(use-package hydra
  :ensure t)

(use-package keybinding
  :demand t
  :init
  (global-unset-key (kbd "M-m"))
  :config
  (progn
    (rename-mnemonic-key-prefix "g" "VCS")
    (rename-mnemonic-key-prefix "e" "Errors")
    (rename-mnemonic-key-prefix "p" "Projects")
    (rename-mnemonic-key-prefix "f" "Files")
    (rename-mnemonic-key-prefix "b" "Buffers")
    (rename-mnemonic-key-prefix "w" "Windows")
    (rename-mnemonic-key-prefix "s" "Search/Replace")
    (rename-mnemonic-key-prefix "sr" "Replace")
    (rename-mnemonic-key-prefix "!" "Terminal")
    (rename-mnemonic-key-prefix "t" "Tags")))

(use-package key-chord
  :ensure t
  :init
  (progn (setq key-chord-two-keys-delay 0.05))
  :config (key-chord-mode +1))

(use-package smex
  :ensure t
  :defer t)
(use-package ido
  :ensure t
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-use-virtual-buffers t)
    (setq ido-enable-regexp t)

    (add-hook
     'ido-setup-hook
     #'(lambda () (mode-keys
                   ido-completion-map
                   ("<tab>" . 'ido-exit-minibuffer)
                   ("<return>" . 'ido-exit-minibuffer))))

    (ido-mode +1)
    (ido-vertical-mode +1)))
(use-package ido-completing-read+
  :ensure t
  :after ido
  :config
  (ido-ubiquitous-mode +1))
(use-package ido-vertical-mode
  :ensure t
  :after ido
  :config
  (ido-vertical-mode +1))
(use-package flx
  :defer t
  :ensure t)
(use-package flx-ido
  :ensure t
  :after ido
  :after flx
  :config (flx-ido-mode +1))

(use-package counsel
  :ensure t)
(use-package ivy
  :ensure t
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
      ("M-x" . 'counsel-M-x)
      ("C-c M-x" . 'execute-extended-command)
      ("C-x C-f" . 'counsel-find-file))))

(use-package helper-functions
  :demand t)

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package abbrev
  :diminish abbrev-mode)

;; (global-hl-line-mode t)
(use-package hl-line
  :config
  (enable-minor-mode-globally hl-line-mode))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default custom-file "/dev/null")

(use-package hydra
  :ensure t
  :config
  (defhydra windows-hydra ()
    "
^Windows^				^Window^		^Zoom^
--------------------------------------------------------------------------
_<left>_ _h_: windmove-left		_w_: enlarge	_-_: zoom out
_<down>_ _j_: windmove-down		_s_: shrink	_+_ _=_: zoom in
_<up>_ _k_: windmove-up		_a_: widen	_0_: reset
_<right>_ _l_: windmove-right	_d_: tighten	_q_: quit"
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<up>" windmove-up)
    ("<down>" windmove-down)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("+" text-scale-increase)
    ("=" text-scale-increase)
    ("-" text-scale-decrease)
    ("w" enlarge-window)
    ("a" enlarge-window-horizontally)
    ("s" shrink-window)
    ("d" shrink-window-horizontally)
    ("0" (text-scale-increase 0))
    ("q" nil)))

(use-package keybinding
  :config
  (progn
    (prefixed-keys
     ("bb" . 'switch-to-buffer)
     ("bd" . 'kill-this-buffer)
     ("C-i" . #'crux-switch-to-previous-buffer)
     ("bn" . 'next-buffer)
     ("bp" . 'previous-buffer)
     ("ff" . 'counsel-find-file)
     ("wd" . 'delete-window)
     ("wD" . 'delete-other-window)
     ("wh" . 'split-window-horizontally)
     ("wv" . 'split-window-vertically)
     ("ww" . #'windows-hydra/body))

    (global-keys
     ("C-S-j" . #'join-next-line)
     ("C-S-k" . #'join-line)
     ("C-S-y" . #'crux-duplicate-current-line-or-region))
    (global-key "C-x C-b" 'ibuffer)
    (global-key "M-/" 'hippie-expand)

    (global-keys
     ("C-s" . 'isearch-forward-regexp)
     ("C-r" . 'isearch-backward-regexp)
     ("C-M-s" . 'isearch-forward)
     ("C-M-r" . 'isearch-backward))

    (global-key "C-a" #'crux-move-beginning-of-line)
    (global-keys
     ("C-o" . #'crux-smart-open-line)
     ("C-S-o" . #'crux-smart-open-line-above)
     ("C-S-d" . #'crux-kill-whole-line))

    (global-key "C-c =" #'crux-indent-defun)

    (prefixed-key "!!" #'crux-visit-term-buffer)))

(use-package f :ensure t :defer t)
(use-package s :ensure t :defer t)
(use-package dash :ensure t :defer t)

(use-package annoying-arrows-mode
  :ensure t
  :defer 5
  :diminish annoying-arrows-mode
  :config
  (global-annoying-arrows-mode +1))

(use-package beacon
  :ensure t
  :commands beacon-blink
  :init
  (global-key "C-\\" #'beacon-blink))

(use-package crux
  :ensure t)

(show-paren-mode +1)

(use-package rainbow-delimiters
  :ensure t
  :config
  (enable-minor-mode-globally rainbow-delimiters-mode))

(use-package fill-column-indicator
  :ensure t
  :config
  (enable-minor-mode-globally fci-mode))

(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  "Turn off FCI for company mode.
IGNORE: ignore."
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  "Turn on FCI when company mode is off.
IGNORE: ignore."
  (when company-fci-mode-on-p (fci-mode +1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :init
  (progn
    (setq highlight-indent-guides-auto-odd-face-perc 2)
    (setq highlight-indent-guides-auto-even-face-perc 4)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :init
  (global-key "C-=" #'er/expand-region))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode +1))

(use-package company
  :ensure t
  :config
  (progn
    (global-company-mode t)
    (setq company-show-numbers t)))

(use-package company-quickhelp
  :ensure t
  :after company
  :config (company-quickhelp-mode +1))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :config
  (global-eldoc-mode +1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-keymap-prefix (kbd (concat +keybinding/mnemonic-prefix+ " e")))
  :config
  (progn
    (global-flycheck-mode t)))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode +1))

(use-package flyspell
  :ensure t
  :commands (flyspell-mode flyspell-prog-mode)
  :diminish (flyspell-mode . " ")
  :init
  (progn
    (setq ispell-program-name (locate-file "aspell" exec-path))
    (setq ispell-list-command "--list")
    (add-hook 'text-mode-hook #'(lambda () (flyspell-mode +1)))
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package elec-pair
  :commands (electric-pair-mode electric-pair-local-mode)
  :init
  (add-hook 'text-mode-hook #'(lambda () (electric-pair-local-mode +1))))

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
  :diminish (smartparens-mode . " ")
  :config
  (add-hook 'prog-mode-hook #'(lambda () (smartparens-mode +1)))

  ;; Setup smartparens in minibuffer
  (add-hook 'minibuffer-setup-hook #'(lambda () (smartparens-mode +1)))
  (setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (mode-keys smartparens-mode-map
             ;; Strict mode toggle
             ("C-c C-s" . 'smartparens-strict-mode)
             ;; Navigation
             ("C-M-a" . 'sp-beginning-of-sexp)
             ("C-M-e" . 'sp-end-of-sexp)
             ("C-M-f" . 'sp-forward-sexp)
             ("C-M-b" . 'sp-backward-sexp)

             ;; Traversal
             ("C-<down>" . 'sp-down-sexp)
             ("C-<up>" . 'sp-up-sexp)
             ("M-<down>" . 'sp-backward-down-sexp)
             ("M-<up>" . 'sp-backward-up-sexp)
             ("C-M-n" . 'sp-next-sexp)
             ("C-M-p" . 'sp-previous-sexp)
             ("C-S-f" . 'sp-forward-symbol)
             ("C-S-b" . 'sp-backward-symbol)

             ;; AST re-arrange.
             ("C-)" . 'sp-forward-slurp-sexp)
             ;; ("C-)" . 'sp-slurp-hybrid-sexp)
             ("C-}" . 'sp-forward-barf-sexp)
             ("C-(" . 'sp-backward-slurp-sexp)
             ("C-{" . 'sp-backward-barf-sexp)

             ;; Killing
             ("C-M-k" . 'sp-kill-sexp)
             ("C-k" . 'sp-kill-hybrid-sexp)
             ("M-k" . 'sp-backward-kill-sexp)
             ("C-M-<up>" . 'sp-raise-sexp)

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

(use-package electric
  :config
  (electric-indent-mode +1))

(use-package avy
  :ensure t
  :commands avy-goto-word-1
  :init
    (key-chord-define-global "jj" 'avy-goto-word-1))

(use-package compile
  :commands (compile recompile)
  :init
  (prefixed-keys
   ("cc" . 'compile)
   ("cr" . 'recompile)))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode +1))

(use-package savehist
  :commands savehist-mode
  :init
  (add-hook 'after-init-hook 'savehist-mode)
  :config
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
    (setq savehist-file "~/.emacs.d/tmp/history")))

(use-package recentf
  :commands recentf-mode
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  :config
  (progn
    (setq recentf-max-menu-items 25)

    ;; Save recent files every few minutes.
    (run-at-time nil (* 5 60) 'recentf-save-list)

    ;; Silent the saved recent files message
    (silence-function 'recentf-save-list)))

(use-package magit
  :ensure t
  :config
  (progn
    (prefixed-key "gs" 'magit-status)))

(use-package monky
  :ensure t
  :config
  (progn
    (prefixed-key "gh" 'monky-status)))

(use-package diff-hl
  :ensure t
  :config
  (progn
    (enable-minor-mode-globally diff-hl-mode)
    (enable-minor-mode-globally diff-hl-flydiff-mode)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package smerge-mode
  :after hydra
  :after keybinding
  :config
  (progn
    (defhydra hydra-smerge
      (:color green)
      "
^Move^	^Keep^	^Aux^	^Diff^
------------------------------------------------------
_n_ext	_b_ase	_R_efine	_<_: base-upper	_q_uit
_p_rev	_u_pper	_E_diff	_=_: upper-lower	_RET_: current
^ ^	_l_ower	_C_ombine	_>_: base-lower
^ ^	_a_ll	_r_esolve"
      ("RET" smerge-keep-current)
      ("C" smerge-combine-with-next)
      ("E" smerge-ediff)
      ("R" smerge-refine)
      ("a" smerge-keep-all)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("n" smerge-next)
      ("l" smerge-keep-lower)
      ("p" smerge-prev)
      ("r" smerge-resolve)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("q" nil :color red))

    (prefixed-mode-key smerge-mode-map "m" #'hydra-smerge/body)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-keymap-prefix (kbd (concat +keybinding/mnemonic-prefix+ " p"))))
  :config
  (progn
    (projectile-mode +1)
    (setq projectile-completion-system 'ivy)
    (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (yas-global-mode +1)
    (prefixed-key "is" #'yas-expand)))

(use-package irony
  :ensure t
  :commands irony-mode
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode))
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :commands company-irony
  :after company
  :after irony
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :commands company-irony-c-headers
  :after company
  :after irony
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

;; Company-clang doesn't work well with the work setup.
(setq company-backends (delete 'company-clang company-backends))

(use-package flycheck-irony
  :ensure t
  :commands flycheck-irony-setup
  :after flycheck
  :after irony
  :init
  (add-hook 'c-mode-common-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t
  :commands irony-eldoc
  :after irony
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package google-c-style
  :ensure t
  :commands google-set-c-style
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(use-package rtags
  :disabled
  :config
  (progn
    ;; Can't do it since this is not compatible with work. Also, irony-mode is pretty good.
    (setq rtags-completions-enabled nil)

    (setq rtags-autostart-diagnostics t)
    (rtags-enable-standard-keybindings)))

;; Maybe someday.

(use-package company-rtags
  :disabled
  :after company
  :after rtags
  :config
  (add-to-list 'company-backends 'company-rtags))

(defvar lisp-family-mode-hook nil
  "Hook for lisp family major modes.")

(add-hook 'emacs-lisp-mode-hook #'(lambda () (run-hooks 'lisp-family-mode-hook)))
(add-hook 'lisp-mode-hook #'(lambda () (run-hooks 'lisp-family-mode-hook)))

(add-hook 'lisp-family-mode-hook 'smartparens-strict-mode)

(use-package redshank
  :load-path "third_party/redshank"
  :commands redshank-mode
  :diminish redshank-mode
  :init
  (progn
    (add-hook 'lisp-family-mode-hook #'redshank-mode)))

;; Helper functions.
(defun elisp-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(defun elisp-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook
   'after-save-hook
   (lambda ()
     (when (and (file-exists-p (byte-compile-dest-file buffer-file-name)))
       (emacs-lisp-byte-compile)))
   nil
   t))

(defun emacs-lisp-mode-setup ()
  "Setup for emacs-lisp mode."
  (elisp-recompile-elc-on-save)
  (setq mode-name "ELisp"))

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-mode-setup)

(use-package elisp-slime-nav
  :ensure t
  :commands turn-on-elisp-slime-nav-mode
  :diminish elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(mode-keys
 emacs-lisp-mode-map
 ("C-c C-z" . #'elisp-visit-ielm)
 ("C-c C-c" . 'eval-defun)
 ("C-c C-b" . 'eval-buffer)
 ("C-c C-r" . 'eval-region))

(use-package macrostep
  :ensure t
  :commands macrostep-mode
  :init
  (mode-key emacs-lisp-mode-map "C-c m" #'macrostep-mode))

(use-package erefactor
  :ensure t
  :commands (erefactor-map erefactor-lazy-highlight-turn-on)
  :init
  (progn
    (mode-key emacs-lisp-mode-map "C-c C-v" #'erefactor-map)
    (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)))

(use-package litable
  :ensure t
  :commands litable-mode
  :init
  (progn
    (mode-key emacs-lisp-mode-map "C-c l" #'litable-mode)
    (mode-key lisp-interaction-mode-map "C-c l" #'litable-mode))
  :config
  (progn
    (setq litable-list-file "~/.emacs.d/tmp/litable-lists.el")
    (mode-key litable-mode-map "C-c p" #'litable-accept-as-pure)))

(use-package eval-expr
  :ensure t
  ;; Use `pp-eval-expression'. Retain the config for minibuffer setup example.
  :disabled
  :config
  (progn
    (global-key "M-:" #'eval-expr)
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (set (make-local-variable 'eldoc-documentation-function) #'elisp-eldoc-documentation-function)
      (eldoc-mode +1)
      (local-set-key (kbd "<tab>") #'counsel-el))))

(global-key "M-:" 'pp-eval-expression)

(add-hook
 'lisp-interaction-mode-hook
 #'(lambda () (run-hooks 'emacs-lisp-mode-hook)))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (progn
    (defun go-mode-setup ()
      (add-hook 'before-save-hook #'gofmt-before-save)
      (setq gofmt-command "goimports")
      (go-guru-hl-identifier-mode +1))
    (add-hook 'go-mode-hook #'go-mode-setup)
    (mode-key go-mode-map "M-." #'godef-jump)))

(use-package company-go
  :ensure t
  :after company
  :after go-mode
  :commands company-go
  :init
  (add-to-list 'company-backends 'company-go)
  :config
  (setq company-go-show-annotation t))

(use-package flycheck-gometalinter
  :ensure t
  :after flycheck
  :after go-mode
  :commands flycheck-gometalinter-setup
  :config
  (add-hook 'go-mode-hook #'flycheck-gometalinter-setup))

(use-package go-eldoc
  :ensure t
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package go-guru
  :ensure t
  :after go-mode
  :commands go-guru-hl-identifier-mode
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package go-rename
  :ensure t
  :commands go-rename
  :init
  (mode-key go-mode-map "C-c r" #'go-rename))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (mode-keys haskell-mode-map
             ("C-c d" . #'haskell-debug)
             ("C-c i" . #'haskell-interactive-switch)
             ("C-c t" . #'haskell-process-do-type)
             ("C-c h" . #'haskell-process-do-info)
             ("C-c fi" . #'haskell-add-import)
             ("C-c ff" . #'haskell-mode-stylish-buffer)))

(use-package hindent
  :ensure t
  :commands hindent-mode
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hlint-refactor
  :ensure t
  :commands hlint-refactor-mode
  :init
  (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

(use-package shm
  :ensure t
  :commands structured-haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'structured-haskell-mode))

(use-package intero
  :ensure t
  :commands intero-mode
  :init
  (add-hook 'haskell-mode-hook #'intero-mode))

(use-package company-ghc
  :ensure t
  :commands company-ghc
  :init
  (add-to-list 'company-backends #'company-ghc))

(use-package company-ghci
  :ensure t
  :commands company-ghci
  :init
  (add-to-list 'company-backends #'company-ghci))

(use-package flycheck-haskell
  :ensure t
  :after flycheck
  :commands flycheck-haskell-setup
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

(add-to-list 'flycheck-ghc-search-path (expand-file-name "~/.xmonad/lib"))

(use-package meghanada
  :ensure t
  :commands meghanada-mode
  :init
  (progn
    (add-hook 'java-mode-hook #'(lambda () (meghanada-mode +1)))))

(add-hook 'java-mode-hook #'(lambda () (setq fill-column 100)))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'"
  :init
  (add-hook
   'protobuf-mode-hook
   #'(lambda ()
       (setq
        imenu-generic-expression
        '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :diminish anaconda-mode
  :diminish anaconda-eldoc-mode
  :init
  (add-hook 'python-mode-hook #'(lambda () (anaconda-mode +1))))

(use-package company-anaconda
  :ensure t
  :commands company-anaconda
  :init
  (add-to-list 'company-backends 'company-anaconda))

(use-package virtualenvwrapper
  :disabled
  :config
  (progn
    (setq eshell-prompt-function
       (lambda () (concat venv-current-name " $ ")))
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)))

(use-package pyenv-mode
  :ensure t
  :commands pyenv-mode
  :after virtualenvwrapper
  :init
  (add-hook 'python-mode-hook #'(lambda () (pyenv-mode +1))))

(use-package py-yapf
  :commands py-yapf
  :ensure t)

(use-package pytest
  :commands pytest
  :ensure t)

(defconst +zsh-filename-patterns+
  '("\\.zsh\\'"
    "zlogin\\'"
    "zlogout\\'"
    "zpreztorc\\'"
    "zprofile\\'"
    "zshenv\\'"
    "zshrc\\'")
  "Filename patterns for Zsh script files.")

(use-package sh-script
  :init
  (progn
    (dolist (pattern +zsh-filename-patterns+)
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
    (add-hook
     'sh-mode-hook
     #'(lambda ()
       (when
           (and
            buffer-file-name
            (cl-mapcar #'(lambda (pat) (string-match-p pat buffer-file-name)) +zsh-filename-patterns+))
         (sh-set-shell "zsh"))))))

(use-package company-shell
  :ensure t
  :commands company-shell
  :after company
  :init
  (add-hook
   'sh-mode-hook
   #'(lambda ()
     (add-to-list
      (make-local-variable 'company-backends)
      'company-shell))))

(use-package insert-shebang
  :ensure t
  :config
  (progn
    ;; Don't insert shebang proactively.
    (remove-hook 'find-file-hook 'insert-shebang)))

(use-package ess
  :disabled
  :config
  (progn
    (mode-keys
     inferior-ess-mode-map
     ("C-<up>". 'comint-previous-matching-input-from-input)
     ("C-<down>" . 'comint-next-matching-input-from-input)
     ("C-x t" . 'comint-dynamic-complete-filename))))

(add-hook
 'org-mode-hook
 #'(lambda () (mode-keys
               org-mode-map
               ("C-<up>" . 'org-move-subtree-up)
               ("C-<down>" . 'org-move-subtree-down))))

(setq org-agenda-files '("~/organizer/main.org"))

(setq
 org-agenda-custom-commands
 '(("c" "GTD Agenda View"
    ((agenda "")
     (alltodo "")))))

;; Add this above for high priority task
;; (tags
;;  "PRIORITY=\"A\""
;;  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;   (org-agenda-overriding-header "High-priority unfinished tasks:")))

(defun load-org-gtd-agenda ()
  "Load custom agenda directly."
  (interactive)
  (org-agenda nil "c"))

(global-key "<f2>" #'load-org-gtd-agenda)
(global-key "C-c a" #'load-org-gtd-agenda)

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :diminish org-bullets-mode
  :init
  (add-hook
   'org-mode-hook
   #'(lambda () (org-bullets-mode +1))))

(use-package org-indent
  :commands org-indent-mode
  :diminish org-indent-mode
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-indent-mode +1))))

(setq org-capture-templates
      '(("a" "Action Item" entry (file+headline "~/organizer/main.org" "Action Items")
         "* TODO [#B] %?\n  %i")
        ("c" "Calendar" entry (file+headline "~/organizer/main.org" "Calendar")
         "* %?\n %^T\n %i")
        ("r" "Reference" entry (file "~/organizer/reference.org")
         "* %?\n  %i\n%^{prompt|Description}\n\n:PROPERTIES:\n:RecordDate:\t%T\n:END:"
         :prepend t
         :empty-lines 1)))

(global-key "<f6>" 'org-capture)
(global-key "C-c c" 'org-capture)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

(diminish 'org-src-mode " ")

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(use-package noflet
  :commands noflet
  :ensure t)

(defun make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(use-package stickyfunc-enhance
  :ensure t
  :defer t)

(use-package srefactor
  :ensure t
  :defer t)

(use-package semantic
  :commands semantic-mode
  :init
  (progn
    (setq semantic-default-submodes
          '( ;; Perform semantic actions during idle time
            global-semantic-idle-scheduler-mode
            ;; Use a database of parsed tags
            global-semanticdb-minor-mode
            ;; Decorate buffers with additional semantic information
            global-semantic-decoration-mode
            ;; Highlight the name of the function you're currently in
            global-semantic-highlight-func-mode
            ;; show the name of the function at the top in a sticky
            global-semantic-stickyfunc-mode
            ;; Generate a summary of the current tag when idle
                                        ; global-semantic-idle-summary-mode

            ;; Show a breadcrumb of location during idle time
            global-semantic-idle-breadcrumbs-mode
            ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
            ;; or `C-x B'
            global-semantic-mru-bookmark-mode))

    (add-hook 'emacs-lisp-mode-hook 'semantic-mode)
    (add-hook 'python-mode-hook 'semantic-mode)
    (add-hook 'java-mode-hook 'semantic-mode)
    (add-hook 'c-mode-hook 'semantic-mode)
    ;; etc etc
    (add-hook 'prog-mode-hook 'semantic-mode)))

(use-package which-func
  :disabled
  :config
  (progn
    (which-function-mode +1)
    (setq which-func-unknown "")))

(prefixed-key "tt" 'counsel-imenu)

(use-package ag
  :ensure t
  :defer 5)

(prefixed-key "ss" 'swiper)
(key-chord-define-global "??" 'swiper)

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (progn
    (global-anzu-mode +1)
    (global-set-key [remap query-replace] #'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)
    (prefixed-keys
     ("srr" . #'anzu-query-replace-regexp)
     ("sr." . #'anzu-query-replace-at-cursor-thing))))

(use-package iedit
  :commands iedit-dwim
  :init
  (progn
    (defun iedit-dwim (arg)
      "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
      (interactive "P")
      (if arg
          (iedit-mode)
        (save-excursion
          (save-restriction
            (widen)
            ;; this function determines the scope of `iedit-start'.
            (if iedit-mode
                (iedit-done)
              ;; `current-word' can of course be replaced by other
              ;; functions.
              (narrow-to-defun)
              (iedit-start (current-word) (point-min) (point-max)))))))

    (prefixed-key "sri" #'iedit-dwim)))

(use-package evil
  :ensure t
  :defer 10)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package powerline :ensure t :defer t)
(use-package let-alist :ensure t :defer t)
(use-package all-the-icons :ensure t :defer t)

(use-package atom-one-dark-theme
  :ensure t
  :config
  (progn
    (load-theme 'atom-one-dark t)
    (set-frame-font "Inconsolata-18")))
  ;; (setq default-frame-alist '((font . "Inconsolata-18")))))

(use-package spaceline
  :disabled
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package smart-mode-line
  :disabled
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package helium-modeline
  :after powerline
  :after let-alist
  :after projectile
  :after flycheck
  :after window-numbering
  :after
  :config
  (powerline-helium-theme))

(use-package theme-enhancement
  :config
  (theme-enhancement/apply))

(use-package linum
  :config
  (progn
    (global-linum-mode)
    (setq-default linum-format " %4d ")

    (set-face-attribute
     'linum
     nil
     ;; :background "#282a2e"
     :bold nil
     :weight 'normal
     :height 0.9
     :slant 'normal
     :box nil)))

(load-file "~/.emacs.machine.el")

(provide 'config)
