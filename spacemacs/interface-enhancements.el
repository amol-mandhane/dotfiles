;; Don't look horrible on Mac.
(setq ns-use-srgb-colorspace nil)

;; TextMate style newline characters
(setq vi-tilde-fringe-bitmap-array [0 0 0 3 3 255 0 0])

;; I learn a lot of new things from menu bar.
(menu-bar-mode 1)

;; Enable fuzzy matching everywhere in Helm. Because, why not?
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

;; Make sure FCI doesn't mess up auto-completion
(defun config/fci-enabled-p () (symbol-value 'fci-mode))

(defvar config/fci-mode-suppressed nil)
(make-variable-buffer-local 'config/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (config/fci-enabled-p)))
    (when fci-enabled
      (setq config/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and config/fci-mode-suppressed
             (null popup-instances))
    (setq config/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(provide 'interface-enhancements)
