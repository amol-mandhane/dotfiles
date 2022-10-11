;;; config-editor.el --- Editor config               -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)
(require 'helper-lib)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq user-full-name ""
      user-mail-address ""

      inhibit-startup-screen t
      initial-scratch-message ";;; Lisp Interaction Mode\n"
      initial-major-mode 'lisp-interaction-mode

      ad-redefinition-action 'accept

      ns-use-srgb-colorspace nil
      ns-use-thin-smoothing t
      mac-command-modifier 'control

      save-interprogram-paste-before-kill t
      apropos-do-all t)

(setq-default indent-tabs-mode nil
              fill-column 80

              custom-file "/dev/null")

(prefer-coding-system 'utf-8)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config (setq auto-revert-verbose nil))

(use-package ibuffer
  :straight nil
  :bind (("C-x C-b" . ibuffer)))

(use-package exec-path-from-shell
  :straight t
  :hook (after-init . exec-path-from-shell-initialize))

(use-package register
  :prefixed-bind (("rr" . copy-to-register)
                  ("ri" . insert-register)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-file "~/.emacs.d/historyfile"))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-menu-items 25)
  (run-at-time
   nil (* 5 60)
   (lambda () (let ((save-silently t))
                (recentf-save-list)))))

(use-package crux
  :straight t
  :bind (("C-S-y" . crux-duplicate-current-line-or-region)
         ("C-a" . crux-move-beginning-of-line)
         ("C-S-d" . crux-kill-whole-line)
         ("C-S-j" . join-next-line)
         ("C-c =" . crux-indent-defun))
  :prefixed-bind (("!!" . crux-visit-term-buffer)))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package electric
  :hook (after-init . electric-indent-mode))

(use-package paren
  :hook (after-init . show-paren-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :hook (text-mode . whitespace-mode)
  :hook (before-save . delete-trailing-whitespace)
  :init
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80
        require-final-newline t))

(use-package hungry-delete
  :straight t
  :hook (after-init . global-hungry-delete-mode))

(use-package yasnippet
  :straight t
  :hook (after-init . yas-global-mode)
  :prefixed-bind ("is" . yas-expand))

(use-package undo-tree
  :straight t
  :hook (after-init . global-undo-tree-mode)
  :bind (:map undo-tree-visualizer-mode-map
              ("<RET>" . undo-tree-visualizer-quit)))

;; (use-package annoying-arrows-mode
;;   :straight t
;;   :hook (after-init . global-annoying-arrows-mode))

;; (defconst disable-mouse--bindings-targets '("bottom-divider" "vertical-line"))
;; (use-package disable-mouse
;;   :straight t
;;   :hook (after-init . global-disable-mouse-mode)
;;   :config
;;   (defconst disable-mouse--button-events '("up-mouse" "down-mouse" "drag-mouse"))
;;   (defconst disable-mouse--bindings-targets '("bottom-divider" "vertical-line"))
;;   (setq disable-mouse-wheel-events nil))

(use-package elec-pair
  :hook (text-mode . electric-pair-local-mode))

(use-package smartparens-config
  :straight smartparens
  :demand t
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
              ("C-M-<up>" . sp-raise-sexp))
  :config
  (require 'smartparens-config)
  (cl-delete 'minibuffer-inactive-mode sp-ignore-modes-list)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

(use-package helm
  :straight t
  :demand t
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
  (require 'helm-config)
  (setq
   completion-styles
   `(basic partial-completion emacs22 initials
           ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
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
  (helm-autoresize-mode +1))

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
  :straight t
  :hook (after-init . window-numbering-mode)
  :config
  (defun window-numbering-get-number-string (&optional w)
    (let ((s (int-to-string (window-numbering-get-number w))))
      (propertize
       (concat " " s " ")
       'face (if (moody-window-active-p)
                 '(:background "tomato1" :inherit window-numbering-face
                   :box (:line-width 4 :color "tomato1"))
               '(:background "darkgrey" :inherit window-numbering-face
                 :box (:line-width 4 :color "darkgrey")))))))

(use-package window-purpose
  :straight t
  :demand t
  :config
  (add-to-list 'purpose-user-name-purposes '("*Eldoc help*" . documentation))
  (add-to-list 'purpose-user-name-purposes '("*Ilist*" . ilist))
  (add-to-list 'purpose-user-mode-purposes
               '(flymake-diagnostics-buffer-mode . diagnostics))
  (setq purpose-use-default-configuration t
        purpose-preferred-prompt 'helm)
  (define-key purpose-mode-map (kbd "C-x C-f") nil)
  (define-key purpose-mode-map (kbd "C-x b") nil)
  (purpose-compile-user-configuration)
  (purpose-mode +1))

(provide 'config-editor)
;;; config-editor.el ends here
