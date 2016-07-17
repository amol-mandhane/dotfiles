;; Don't look horrible on Mac.
(setq ns-use-srgb-colorspace nil)

;; TextMate style newline characters
(setq vi-tilde-fringe-bitmap-array [0 0 0 3 3 255 0 0])

;; I learn a lot of new things from menu bar.
(menu-bar-mode 1)

;; Enable fuzzy matching everywhere in Helm. Because, why not?
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(provide 'interface-enhancements)
