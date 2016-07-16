;; Java
; Java has big words. Use 100 character limit.
(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))

;; company with eclim sucks. Use auto-complete.
(add-hook 'java-mode-hook (lambda () (if (bound-and-true-p company-mode) (company-mode -1))) t)
(add-hook 'java-mode-hook 'auto-complete-mode)
(add-to-list 'ac-modes 'java-mode)


(provide 'lang-config)
