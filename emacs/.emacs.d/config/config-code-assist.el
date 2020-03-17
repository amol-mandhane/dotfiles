;;; config-code-assist.el --- Code assistance features  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)
(require 'config-keybinding)

(use-package projectile
  :straight t
  :after (helm)
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-enable-caching t
        projectile-keymap-prefix (kbd
                                  (concat +keybinding/mnemonic-prefix+ " p")))
  :config
  (setq projectile-completion-system 'helm
        projectile-mode-line-lighter " P"))

(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :hook (after-init . helm-projectile-on)
  :prefixed-bind (("pp" . helm-projectile)))

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :config
  (setq company-show-numbers t
        company-dabbrev-downcase nil)
  (fmakunbound 'company-semantic)
  (fmakunbound 'company-clang))

(use-package company-quickhelp
  :straight t
  :hook (after-init . company-quickhelp-mode))

(use-package company-statistics
  :straight t
  :hook (after-init . company-statistics-mode))

(use-package eldoc
  :hook (after-init . global-eldoc-mode)
  :config
  (defun eldoc-custom-frontend (format-string &rest args)
    "Display doc in a buffer if visible, else in echo area."
    (let* ((eldoc-buffer (get-buffer-create "*Eldoc help*"))
           (buffer-visible (and eldoc-buffer (get-buffer-window eldoc-buffer))))
      (if (not buffer-visible)
          (apply #'eldoc-minibuffer-message format-string args)
        (with-current-buffer eldoc-buffer
          (setq-local buffer-read-only nil)
          (erase-buffer)
          (when format-string
            (insert (apply #'format format-string args)))
          (setq-local buffer-read-only t)))))
  (setq eldoc-message-function #'eldoc-custom-frontend))

(use-package compile
  :prefixed-bind (("cc" . compile)
                  ("cr" . recompile)))

(defvar config-flycheck-modes '(emacs-lisp-mode
                                c-mode
                                c++-mode
                                go-mode
                                haskell-mode)
  "Major modes to enable Flycheck in.

Other modes will use Flymake.")

(use-package flycheck
  :straight t
  :commands flycheck-mode
  :init
  (setq
   flycheck-keymap-prefix (kbd (concat +keybinding/mnemonic-prefix+ " e")))
  (add-hook
   'prog-mode-hook
   (lambda ()
     (when (memq major-mode config-flycheck-modes) (flycheck-mode +1))))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :straight t
  :disabled
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flymake
  :commands flymake-mode
  :init
  (add-hook
   'prog-mode-hook
   (lambda ()
     (unless (memq major-mode config-flycheck-modes) (flymake-mode +1)))))

(use-package flymake-diagnostic-at-point
  :straight t
  :after (flymake)
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package semantic
  :hook (prog-mode . semantic-mode)
  :init
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
          global-semantic-mru-bookmark-mode)))

(use-package stickyfunc-enhance
  :straight t
  :after (semantic-mode)
  :demand t)

(use-package eglot
  :straight t
  :commands (eglot-ensure))

(provide 'config-code-assist)
;;; config-code-assist.el ends here