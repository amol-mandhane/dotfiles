(setq user-full-name ""
      user-mail-address "")

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
(setq visible-bell nil
      ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer
           0.1 nil
           (lambda (fg) (set-face-foreground 'mode-line fg))
           orig-fg))))

(setq ns-use-srgb-colorspace nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)
(column-number-mode -1)
(size-indication-mode -1)

(unless (frame-parameter nil 'fullscreen)
  (if
      (eval-when-compile
        (eq system-type 'darwin))
      (toggle-frame-fullscreen)
    (toggle-frame-maximized)))

(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq-default custom-file "/dev/null")

(add-to-list 'load-path (eval-when-compile (expand-file-name "~/.emacs.d/lisp")))

(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config
  (progn
    (key-chord-mode +1)
    (setq key-chord-two-keys-delay 0.05)))

(use-package use-package-prefixed-bind)

(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . exec-path-from-shell-initialize))

(add-hook 'after-init-hook (lambda () (server-start)))

(use-package f :ensure t :defer t)
(use-package s :ensure t :defer t)
(use-package dash :ensure t :defer t)
(use-package powerline :ensure t :defer t)
(use-package let-alist :ensure t :defer t)
(use-package all-the-icons :ensure t :defer t)
(use-package hydra :ensure t :demand t)
(use-package helper-functions :demand t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (progn
    (setq which-key-sort-order 'which-key-key-order-alpha
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.5)

    (defun rename-mnemonic-key-prefix (key-string name)
      (which-key-add-key-based-replacements
        (concat +keybinding/mnemonic-prefix+ " " key-string) name))

    (rename-mnemonic-key-prefix "!" "Terminal")
    (rename-mnemonic-key-prefix "b" "Buffers")
    (rename-mnemonic-key-prefix "c" "Compilation")
    (rename-mnemonic-key-prefix "e" "Errors")
    (rename-mnemonic-key-prefix "f" "Files")
    (rename-mnemonic-key-prefix "g" "VCS")
    (rename-mnemonic-key-prefix "p" "Projects")
    (rename-mnemonic-key-prefix "r" "Ring/Register")
    (rename-mnemonic-key-prefix "s" "Search/Replace")
    (rename-mnemonic-key-prefix "sr" "Replace")
    (rename-mnemonic-key-prefix "t" "Tags")
    (rename-mnemonic-key-prefix "w" "Windows")))

(use-package helm
  :ensure t
  :demand
  :diminish helm-mode
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-s o" . helm-occur)
         :map helm-map
         ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
         ([tab] . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :prefixed-bind (("bb" . helm-mini)
                  ("ry" . helm-show-kill-ring)
                  ("ff" . helm-find-files)
                  ("tt" . helm-semantic-or-imenu))
  :config
  (progn
    (require 'helm-config)
    (setq helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-scroll-amount 8
          helm-ff-file-name-history-use-recentf t
          ;; helm-echo-input-in-header-line t

          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-session-fuzzy-match t
          helm-etags-fuzzy-match t
          helm-mode-fuzzy-match t
          helm-completion-in-region-fuzzy-match t
          helm-candidate-number-limit 100

          helm-autoresize-min-height 24
          helm-autoresize-max-height 24

          helm-quick-update t
          helm-ff-skip-boring-files t)

    (helm-mode +1)
    (helm-autoresize-mode +1)))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :hook (after-init . helm-projectile-on)
  :prefixed-bind (("pp" . helm-projectile)))

(use-package helm-descbinds
  :ensure t
  :hook (after-init . helm-descbinds-mode))

(use-package helm-ag
  :ensure t
  :commands (helm-ag))

(use-package autorevert
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil))

(use-package abbrev
  :diminish abbrev-mode)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package annoying-arrows-mode
  :ensure t
  :defer 5
  :diminish annoying-arrows-mode
  :commands global-annoying-arrows-mode
  :config
  (global-annoying-arrows-mode +1))

(use-package beacon
  :ensure t
  :bind (("C-\\" . beacon-blink)))

(use-package crux
  :after (helper-functions)
  :commands (crux-eval-and-replace)
  :ensure t
  :bind (("C-S-j" . join-next-line)
         ("C-S-k" . join-line)
         ("C-S-y" . crux-duplicate-current-line-or-region)
         ("C-a" . crux-move-beginning-of-line)
         ("C-S-d" . crux-kill-whole-line)
         ("C-c =" . crux-indent-defun))
  :prefixed-bind (("!!" . crux-visit-term-buffer)))

(use-package paren
  :hook (after-init . show-paren-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :hook (prog-mode . whitespace-mode)
  :hook (text-mode . whitespace-mode)
  :hook (before-save . delete-trailing-whitespace)
  :init
  (progn
    (setq whitespace-style '(face lines-tail))
    (setq whitespace-line-column 80)
    (setq-default require-final-newline t)))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (progn
    (setq highlight-indent-guides-auto-odd-face-perc 2)
    (setq highlight-indent-guides-auto-even-face-perc 4)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :hook (after-init . global-hungry-delete-mode))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package disable-mouse
  :ensure t
  :diminish disable-mouse-mode
  :diminish disable-mouse-global-mode
  :hook (after-init . global-disable-mouse-mode))

(use-package writegood-mode
  :disabled
  :ensure t
  :hook (text-mode . writegood-mode))

(use-package artbollocks-mode
  :ensure t
  :hook (text-mode . artbollocks-mode))

(use-package register
  :prefixed-bind (("rr" . copy-to-register)
                  ("ri" . insert-register)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :prefixed-bind ("is" . yas-expand))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (:map undo-tree-visualizer-mode-map
              ("<RET>" . undo-tree-visualizer-quit)))

(use-package isearch
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-M-r" . isearch-backward)))

(use-package ag
  :ensure t
  :defer 5)

(use-package swiper-helm
  :ensure t
  :after helm
  :chords (("??" . swiper-helm))
  :prefixed-bind ("ss" . swiper-helm))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :hook (after-init . global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :prefixed-bind (("srr" . anzu-query-replace-regexp)
                  ("sr." . anzu-query-replace-at-cursor-thing)))

(use-package wgrep
  :ensure t
  :defer 5)

(use-package wgrep-ag
  :ensure t
  :defer 5)

(use-package iedit
  :ensure t
  :bind (("C-'" . iedit-mode)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :defines (company-dabbrev-downcase)
  :config
  (progn
    (setq company-show-numbers t)
    (setq company-dabbrev-downcase nil)))

(use-package company-quickhelp
  :ensure t
  :hook (after-init . company-quickhelp-mode))

(use-package windmove
  :prefixed-bind
  (("bd" . kill-this-buffer)
   ("bn" . next-buffer)
   ("bp" . previous-buffer)

   ("wd" . delete-window)
   ("wD" . delete-other-window)
   ("wh" . split-window-horizontally)
   ("wv" . split-window-vertically)

   ("C-i" . switch-to-previous-buffer)

   ("ww" . windows-hydra/body))
  :init
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

(use-package window-numbering
  :ensure t
  :hook (after-init . window-numbering-mode))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :hook (after-init . global-eldoc-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flycheck-keymap-prefix (kbd (concat +keybinding/mnemonic-prefix+ " e"))))

(use-package flycheck-pos-tip
  :ensure t
  :hook (after-init . flycheck-pos-tip-mode))

(use-package flyspell
  :ensure t
  :after (exec-path-from-shell)
  :diminish (flyspell-mode . " ")
  :hook (text-mode . flyspell-mode)
  :hook (prog-mode . flyspell-prog-mode)
  :init
  (progn
    (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-list-commaqnd "--list")))

(use-package flyspell-correct-helm
  :ensure t
  :after (flyspell helm)
  :bind (:map flyspell-mode-map
              ("C-c C-\\" . flyspell-correct-previous-word-generic)))

(use-package elec-pair
  :hook (text-mode . electric-pair-local-mode))

(use-package smartparens-config
  :ensure smartparens
  :demand t
  :diminish (smartparens-mode . " ")
  :hook (prog-mode . smartparens-mode)
  :hook (minibuffer-setup . smartparens-mode)
  :bind (:map smartparens-mode-map
              ;; Strict mode toggle
              ("C-c C-s" . smartparens-strict-mode)
              ;; Navigation
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ;; Traversal
              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)
              ("C-M-n"    . sp-next-sexp)
              ("C-M-p"    . sp-previous-sexp)
              ("C-S-f"    . sp-forward-symbol)
              ("C-S-b"    . sp-backward-symbol)

              ;; AST re-arrange.
              ("C-)" . sp-forward-slurp-sexp)
              ;; ("C-)" . 'sp-slurp-hybrid-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)

              ;; Killing
              ("C-M-k"    . sp-kill-sexp)
              ("C-k"      . sp-kill-hybrid-sexp)
              ("M-k"      . sp-backward-kill-sexp)
              ("C-M-<up>" . sp-raise-sexp)

              ;; Unknown
              ("C-M-t" . sp-transpose-sexp)
              ;; ("C-M-w" . sp-copy-sexp)
              ;; ("C-M-d" . delete-sexp)
              ;; ("M-<backspace>" . backward-kill-word)
              ;; ("C-<backspace>" . sp-backward-kill-word)
              ;; ([remap sp-backward-kill-word] . backward-kill-word)
              ;; ("M-[" . sp-backward-unwrap-sexp)
              ;; ("M-]" . sp-unwrap-sexp)
              ;; ("C-x C-t" . sp-transpose-hybrid-sexp)

              ;; Wrap
              ("C-c C-w ("  . (lambda () (interactive) (sp-wrap-with-pair "(")))
              ("C-c C-w ["  . (lambda () (interactive) (sp-wrap-with-pair "[")))
              ("C-c C-w {"  . (lambda () (interactive) (sp-wrap-with-pair "{")))
              ("C-c C-w '"  . (lambda () (interactive) (sp-wrap-with-pair "'")))
              ("C-c C-w \"" . (lambda () (interactive) (sp-wrap-with-pair "\"")))
              ("C-c C-w `"  . (lambda () (interactive) (sp-wrap-with-pair "`"))))
  :config
  (progn
    (cl-delete 'minibuffer-inactive-mode sp-ignore-modes-list)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)))

(use-package electric
  :hook (after-init . electric-indent-mode))

(use-package paredit
  :ensure t
  :disabled t)

(use-package avy
  :ensure t
  :chords (("jj" . avy-goto-word-1)))

(use-package compile
  :prefixed-bind (("cc" . compile)
                  ("cr" . recompile)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
    (setq savehist-file "~/.emacs.d/tmp/history")))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (progn
    (setq recentf-max-menu-items 25)

    ;; Save recent files every few minutes.
    (run-at-time nil (* 5 60) 'recentf-save-list)

    ;; Silent the saved recent files message
    (silence-function 'recentf-save-list)))

(use-package magit
  :ensure t
  :prefixed-bind (("gs" . magit-status)))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-flydiff-mode diff-hl-margin-mode)
  :init
  (progn
    (enable-minor-mode-globally diff-hl-mode)
    (enable-minor-mode-globally diff-hl-flydiff-mode))
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (defhydra diff-hl-hydra (:foreign-keys warn)
    "diff-hl-hydra"
    ("n" diff-hl-next-hunk "Next Hunk")
    ("p" diff-hl-previous-hunk "Previous Hunk")
    ("k" diff-hl-revert-hunk "Kill Hunk")
    ("q" nil "Quit")))

(eval-when-compile
  (unless (>= emacs-major-version 26)
    (defalias 'smerge-keep-upper 'smerge-keep-mine)
    (defalias 'smerge-keep-lower 'smerge-keep-other)
    (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
    (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
    (defalias 'smerge-diff-base-lower 'smerge-diff-base-other)))

(use-package smerge-mode
  :demand
  :prefixed-bind (:map smerge-mode-map
                       ("m" . hydra-smerge/body))
  :init
  (defhydra hydra-smerge
    (:foreign-keys warn)
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
    ("q" nil :color red)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-keymap-prefix (kbd (concat +keybinding/mnemonic-prefix+ " p"))))
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))))

(use-package irony
  :disabled
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
  :disabled
  :ensure t
  :commands company-irony
  :after (company irony)
  :init
  (add-to-list 'company-backends 'company-irony)
  (add-hook
   'c++-mode-hook
   #'(lambda ()
       (setq-local company-backends (delete 'company-clang company-backends)))))

(use-package company-irony-c-headers
  :disabled
  :ensure t
  :commands company-irony-c-headers
  :after (company irony)
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

;; Company-clang doesn't work well with the work setup.

(use-package flycheck-irony
  :disabled
  :ensure t
  :commands flycheck-irony-setup
  :after (flycheck irony)
  :init
  (add-hook 'c-mode-common-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :disabled
  :ensure t
  :commands irony-eldoc
  :after irony
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package google-c-style
  :ensure t
  :hook (c-mode-common . google-set-c-style))

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
  :after (company rtags)
  :config
  (add-to-list 'company-backends 'company-rtags))

(defvar lisp-family-mode-hook nil
  "Hook for lisp family major modes.")

(add-hook 'emacs-lisp-mode-hook #'(lambda () (run-hooks 'lisp-family-mode-hook)))
(add-hook 'lisp-mode-hook #'(lambda () (run-hooks 'lisp-family-mode-hook)))

(add-hook 'lisp-family-mode-hook #'smartparens-strict-mode)

(use-package redshank
  :ensure t
  :after paredit
  :diminish redshank-mode
  :hook (lisp-family-mode . redshank-mode))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m" . macrostep-mode)
              :map lisp-mode-map
              ("C-c m" . macrostep-mode)))

;; Helper functions.
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
      (setq mode-name "ELisp"))))

(use-package elisp-slime-nav
  :ensure t
  :after (elisp-mode)
  :diminish elisp-slime-nav-mode
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :hook (ielm-mode . turn-on-elisp-slime-nav-mode))

(use-package litable
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c l" . litable-mode)
              :map lisp-interaction-mode-map
              ("C-c l" . litable-mode)
              :map litable-mode-map
              ("C-c p" . litable-accept-as-pure))

  :config
  (setq litable-list-file "~/.emacs.d/tmp/litable-lists.el"))

(use-package eval-expr
  :ensure t
  ;; Use `pp-eval-expression'. Retain the config for minibuffer setup example.
  :disabled
  :bind (("M-:" . eval-expr))
  :config
  (progn
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (setq-local
       eldoc-documentation-function #'elisp-eldoc-documentation-function)
      (eldoc-mode +1))))

(use-package pp
  :bind (("M-:" . pp-eval-expression)))

(add-hook
 'lisp-interaction-mode-hook
 #'(lambda () (run-hooks 'emacs-lisp-mode-hook)))

(use-package slime
  :ensure t
  :hook (common-lisp-mode . slime-mode)
  :functions (slime-toggle-fancy-trace slime-inspect-definition)
  :bind (:map lisp-mode-map
              ("C-c '" . slime)

              ("C-c cc" . slime-compile-file)
              ("C-c cC" . slime-compile-and-load-file)
              ("C-c cl" . slime-load-file)
              ("C-c cf" . slime-compile-defun)
              ("C-c cr" . slime-compile-region)
              ("C-c cn" . slime-remove-notes)

              ("C-c eb" . slime-eval-buffer)
              ("C-c ef" . slime-eval-defun)
              ("C-c eF" . slime-undefine-function)
              ("C-c ee" . slime-eval-last-expression)
              ("C-c er" . slime-eval-region)

              ("C-c gb" . slime-pop-find-definition-stack)
              ("C-c gn" . slime-next-note)
              ("C-c gN" . slime-previous-note)

              ("C-c ha" . slime-apropos)
              ("C-c hA" . slime-apropos-all)
              ("C-c hd" . slime-disassemble-symbol)
              ("C-c hh" . slime-describe-symbol)
              ("C-c hH" . slime-hyperspec-lookup)
              ("C-c hi" . slime-inspect-definition)
              ("C-c hp" . slime-apropos-package)
              ("C-c ht" . slime-toggle-trace-fdefinition)
              ("C-c hT" . slime-untrace-all)
              ("C-c h<" . slime-who-calls)
              ("C-c h>" . slime-calls-who)
              ("C-c hr" . slime-who-references)
              ("C-c hm" . slime-who-macroexpands)
              ("C-c hs" . slime-who-specializes)

              ("C-c Ma" . slime-macroexpand-all)
              ("C-c Mo" . slime-macroexpand-1)

              ("C-c se" . slime-eval-last-expression-in-repl)
              ("C-c si" . slime)
              ("C-c sq" . slime-quit-lisp)

              ("C-c tf" . slime-toggle-fancy-trace))
  :config
  (progn
    (require 'slime-fancy-trace)
    (require 'slime-fancy-inspector)
    (setq inferior-lisp-program "sbcl")
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch
                           slime-company))

    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)

    (add-hook 'slime-repl-mode-hook #'turn-off-smartparens-mode)

    (slime-setup '(slime-repl))

    ))

(use-package slime-company
  :ensure t
  :after (company)
  :commands (company-slime)
  :init
  (add-to-list 'company-backends 'company-slime)
  :config
  (setq slime-company-completion 'fuzzy))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (progn
    (defun go-mode-setup ()
      (add-hook 'before-save-hook #'gofmt-before-save)
      (setq-local tab-width 2)
      (setq gofmt-command "goimports")
      (go-guru-hl-identifier-mode +1))
    (add-hook 'go-mode-hook #'go-mode-setup)))

(use-package company-go
  :ensure t
  :after (company go-mode)
  :commands company-go
  :init
  (add-to-list 'company-backends 'company-go)
  :config
  (setq company-go-show-annotation t))

(use-package flycheck-gometalinter
  :ensure t
  :after (flycheck go-mode)
  :hook (go-mode . flycheck-gometalinter-setup))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(use-package go-rename
  :if (executable-find "gorename")
  :bind (:map go-mode-map
              ("C-c r" . go-rename)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-c d" . haskell-debug)
              ("C-c i" . haskell-interactive-switch)
              ("C-c t" . haskell-process-do-type)
              ("C-c h" . haskell-process-do-info)
              ("C-c fi" . haskell-add-import)
              ("C-c ff" . haskell-mode-stylish-buffer))
  :config
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indent))

(use-package hindent
  :ensure t
  :if (executable-find "hindent")
  :hook (haskell-mode . hindent-mode)
  :config
  ;; reformat the buffer using hindent on save
  (setq hindent-reformat-buffer-on-save t))

(use-package intero
  :after (haskell-mode company flycheck)
  :ensure t
  :hook (haskell-mode . intero-mode)
  :config
  (add-to-list 'flycheck-ghc-search-path (expand-file-name "~/.xmonad/lib")))

(use-package cc-mode
  :init
  (add-hook
   'java-mode-hook
   (lambda () (setq fill-column 100
                    whitespace-line-column 100))))

(use-package tex
  :ensure auctex
  :pin gnu
  :config
  (progn
    (setq TeX-parse-self t)  ;; Enable parse on load.
    (setq TeX-auto-save t)  ;; Enable parse on save.

    (setq TeX-PDF-mode t)))

(use-package company-auctex
  :ensure t
  :after (company tex)
  :hook (after-init . company-auctex-init))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

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
  :diminish anaconda-mode
  :diminish anaconda-eldoc-mode
  :hook (python-mode . anaconda-mode))

(use-package company-anaconda
  :ensure t
  :after (anaconda-mode company)
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
  :hook (python-mode . pyenv-mode)
  :after virtualenvwrapper)

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
  :commands (sh-set-shell)
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

(use-package org
  :ensure t
  :pin gnu
  :bind (:map org-mode-map
              ("C-<up>" . org-move-subtree-up)
              ("C-<down>" . org-move-subtree-down))
  :config
  (progn
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-use-outline-path t)))

(use-package org-agenda
  :after (org)
  :functions (org-agenda)
  :bind (([f2] . load-org-gtd-agenda)
         ("C-c a" . load-org-gtd-agenda))
  :init
  (defun load-org-gtd-agenda ()
    "Load custom agenda directly."
    (interactive)
    (org-agenda nil "c"))
  :config
  (progn
    (setq org-agenda-files '("~/organizer/main.org"))
    (setq org-agenda-custom-commands '(("c" "GTD Agenda View"
                                        ((agenda "")
                                         (alltodo "")))))))

(use-package org-bullets
  :after (org)
  :ensure t
  :diminish org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package org-indent
  :diminish org-indent-mode
  :hook (org-mode . org-indent-mode))

(use-package org-capture
  :after (org)
  :bind (([f6] . org-capture)
         ("C-c c" . org-capture))
  :init
  (setq org-capture-templates
        '(("a" "Action Item" entry (file+headline "~/organizer/main.org" "Action Items")
           "* TODO [#B] %?\n  %i")
          ("c" "Calendar" entry (file+headline "~/organizer/main.org" "Calendar")
           "* %?\n %^T\n %i")
          ("r" "Reference" entry (file "~/organizer/reference.org")
           "* %?\n  %i\n%^{prompt|Description}\n\n:PROPERTIES:\n:RecordDate:\t%T\n:END:"
           :prepend t
           :empty-lines 1))))

(use-package org-src
  :diminish (org-src-mode . " ")
  :config
  (add-hook
   'org-src-mode-hook
   (lambda ()
     (setq-local flycheck-disabled-checkers
                 (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers)))))

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
  :hook (prog-mode . semantic-mode)
  :init
  (progn
    (setq semantic-default-submodes
          '( ;; Perform semantic actions during idle time
            global-semantic-idle-scheduler-mode
            ;; Use a database of parsed tags
            global-semanticdb-minor-mode
            ;; Decorate buffers with additional semantic information
            ;; global-semantic-decoration-mode
            ;; Highlight the name of the function you're currently in
            global-semantic-highlight-func-mode
            ;; show the name of the function at the top in a sticky
            global-semantic-stickyfunc-mode
            ;; Generate a summary of the current tag when idle
            ;; global-semantic-idle-summary-mode

            ;; Show a breadcrumb of location during idle time
            global-semantic-idle-breadcrumbs-mode
            ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
            ;; or `C-x B'
            global-semantic-mru-bookmark-mode))))

(use-package which-func
  :disabled
  :config
  (progn
    (which-function-mode +1)
    (setq which-func-unknown "")))

(use-package epa
  :config
  (progn
    ;; EPA basic config
    (setq epa-file-cache-passphrase-for-symmetric-encryption nil)

    ;; Pinentry config
    (setq epa-pinentry-mode 'loopback)))

(use-package auth-source
  :config
  (setq auth-sources '((:source "~/.emacs.d/.authinfo.gpg"))))

(add-hook
 'after-init-hook
 (lambda ()
   (run-at-time
    "1 min"
    nil
    (lambda () (with-temp-buffer
                 (shell-command "killall gpg-agent" (current-buffer)))))))

(use-package erc-config)
  ;;:hook (after-init . erc-autoconnect-servers))

(use-package elfeed
  :ensure t
  :functions (elfeed-toggle-star)
  :commands (elfeed elfeed-db-load elfeed-search-update--force)
  :bind (([f5] . elfeed-load-db-and-open)
         :map elfeed-search-mode-map
         ("m" . elfeed-toggle-star)
         ("M" . elfeed-toggle-star)
         ("h" . elfeed-hydra/body)
         ("q" . elfeed-save-db-and-bury))
  :init
  ;; Functions to support syncing .elfeed between machines
  ;; makes sure elfeed reads index from disk before launching
  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  :config
  (progn
    ;;write to disk when quiting
    (defun elfeed-save-db-and-bury ()
      "Wrapper to save the elfeed db to disk before burying buffer"
      (interactive)
      (elfeed-db-save)
      (quit-window))

    (defun elfeed--toggle-tag-in-filter (filter tag)
      "Toggles the tag in elfeed search filter."
      (if (s-contains? tag filter)
          (s-replace "  " " " (s-replace tag "" filter))
        (concat filter " " tag)))

    (defun elfeed-filter-toggle-tag (tag)
      "Toggle the tag in active filter."
      (elfeed-search-set-filter (elfeed--toggle-tag-in-filter elfeed-search-filter tag)))

    (setq elfeed-db-directory "~/.elfeed")

    (defhydra elfeed-hydra nil
      "
^Tags^			^Duration^		^Actions^
^^^^^^^---------------------------------------------------------------
_e_macs			_6_: Last 6 months	_U_: Mark as Unread
_c_omics			_T_: Today		_R_: Mark as Read
_m_achine-learning 	_w_: Last week		_b_: Open in browser
_s_cience
_u_nread
"
      ("e" (elfeed-filter-toggle-tag "+emacs"))
      ("u" (elfeed-filter-toggle-tag "+unread"))
      ("c" (elfeed-filter-toggle-tag "+comics"))
      ("m" (elfeed-filter-toggle-tag "+ml"))
      ("s" (elfeed-filter-toggle-tag "+science"))
      ("6" (elfeed-filter-toggle-tag "@6-months-ago"))
      ("T" (elfeed-filter-toggle-tag "@1-day-ago"))
      ("w" (elfeed-filter-toggle-tag "@1-week-ago"))
      ("U" (elfeed-search-tag-all-unread))
      ("R" (elfeed-search-untag-all-unread))
      ("b" elfeed-search-browse-url)
      ("n" next-line)
      ("<down>" next-line)
      ("p" previous-line)
      ("<up>" previous-line)
      ("q" (message "Exit from Elfeed Hydra.") :exit t))

    (defalias 'elfeed-toggle-star
      (elfeed-expose #'elfeed-search-toggle-all 'star))))

(use-package elfeed-org
  :ensure t
  :commands (elfeed-org)
  :init
  (progn
    (setq rmh-elfeed-org-files '("~/.elfeed/feed.org"))
    (add-hook 'after-init-hook #'elfeed-org)))

(use-package elfeed-goodies
  :ensure t
  :hook (after-init . elfeed-goodies/setup))

(use-package challenger-deep-theme
  :ensure t
  :config
  (progn
    (load-theme 'challenger-deep t)
    (set-frame-font "Iosevka-18")))

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
  ;; :requires (powerline let-alist projectile flycheck window-numbering)
  :hook (window-setup . powerline-helium-theme))

(use-package theme-enhancement
  :hook (after-init . (lambda () (theme-enhancement/apply nil :italics :org))))

(use-package display-line-numbers
  :hook (after-init . global-display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-widen t
                display-line-numbers-grow-only t
                display-line-numbers-width 5)
  (set-face-attribute 'line-number-current-line nil :inherit 'fringe))

(load-file "~/.emacs.machine.el")

(provide 'config)
