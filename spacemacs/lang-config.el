;; Java
; Java has big words. Use 100 character limit.
(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))

;; Org mode
(setq org-agenda-files '("~/org-plan"))

(provide 'lang-config)
