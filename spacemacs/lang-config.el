;; Java
; Java has big words. Use 100 character limit.
(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))

;; company with eclim sucks. Use auto-complete.
(add-to-list 'ac-modes 'java-mode)
(setq company-global-modes '(not java-mode))

(provide 'lang-config)
