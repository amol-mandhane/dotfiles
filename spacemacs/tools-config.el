;; YouCompleteMe Daemon
(require 'ycmd)
(set-variable 'ycmd-server-command '("python" "/opt/ycmd/ycmd"))

;; Fix auto-complete
(require 'auto-complete)
(ac-flyspell-workaround)
(setq ac-ignore-case 'smart)

;; $EDITOR config for emacs shell.
(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)

;; Enhancements to semantic mode.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(semantic-mode 1)
(require 'stickyfunc-enhance)

(provide 'tools-config)
