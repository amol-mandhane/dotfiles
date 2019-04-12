;;; config-ui.el --- UI configuration                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'straight)
(require 'use-package)

(defun config-ui-flash-mode-line ()
  "Flash mode-line for a short duration."
  (let ((orig-fg (face-foreground 'mode-line)))
    (set-face-foreground 'mode-line "#F2804F")
    (run-with-idle-timer
     0.1 nil
     (lambda (fg) (set-face-foreground 'mode-line fg)) orig-fg)))

(set-frame-font "Iosevka-10")
(setq-default cursor-type 'bar)

(setq visible-bell nil
      ring-bell-function #'config-ui-flash-mode-line)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode +1)
(line-number-mode +1)
(size-indication-mode +1)
(blink-cursor-mode -1)

(use-package anti-zenburn-theme
  :straight t
  :config
  (load-theme 'anti-zenburn t))

(use-package theme-enhancement
  :after (anti-zenburn-theme)
  :hook (after-init . theme-enhancement/apply))

(use-package minions
  :straight t
  :config
  (minions-mode +1)
  (setq minions-mode-line-lighter "[...]"
        minions-direct '(flymake-mode
                         company-mode
                         flycheck-mode
                         smerge-mode
                         projectile-mode)))

(use-package moody
  :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package display-line-numbers
  ;; Conflicts with flymake diagnostics buffer.
  ;; :hook (after-init . global-display-line-numbers-mode)
  :hook (prog-mode . display-line-numbers-mode)
  :hook (text-mode . display-line-numbers-mode)
  :hook (fundamental-mode . display-line-numbers-mode)
  :init
  (setq display-line-numbers-widen t
        display-line-numbers-grow-only t
        display-line-numbers-width 5)
  (set-face-attribute 'line-number-current-line nil :inherit 'fringe))

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-auto-odd-face-perc 2
              highlight-indent-guides-auto-even-face-perc 4))

(use-package theme-enhancement
  :hook (after-init . (lambda () (theme-enhancement/apply :bold :italics :org))))

(provide 'config-ui)
;;; config-ui.el ends here
