;; Java
; Java has big words. Use 100 character limit.
(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))

(provide 'lang-config)
