(add-to-list 'load-path (expand-file-name "~/.spacemacs.d"))

(setq ns-use-srgb-colorspace nil)

(setq vi-tilde-fringe-bitmap-array [0 0 0 3 3 255 0 0])

(menu-bar-mode 1)

(setq require-final-newline t)

(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(require 'ycmd)
(set-variable 'ycmd-server-command '("python" "/opt/ycmd/ycmd"))

(require 'auto-complete)
(ac-flyspell-workaround)
(setq ac-ignore-case 'smart)

(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(require 'stickyfunc-enhance)

(semantic-mode 1)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))

(setq org-agenda-files '("~/org-plan"))

(add-to-list 'auto-mode-alist '("\\.org.text\\'" . org-mode))

(evil-ex-define-cmd "W" "w")

(require 'smartparens)

(defmacro def-pair (pair)
  `(progn (defun ,(read (concat "wrap-with-"
                           (prin1-to-string (car pair))
                           "s")) (&optional arg)
       (interactive "p")
       (sp-wrap-with-pair ,(cdr pair)))))

(def-pair (paren . "("))
(def-pair (bracket . "["))
(def-pair (brace . "{"))
(def-pair (single-quote . "'"))
(def-pair (double-quote . "\""))
(def-pair (back-quote . "`"))

(bind-keys :map smartparens-mode-map
           ("C-M-a" . sp-beginning-of-sexp)
           ("C-M-e" . sp-end-of-sexp)
           ("C-<down>" . sp-down-sexp)
           ("C-<up>" . sp-up-sexp)
           ("M-<down>" . sp-backward-down-sexp)
           ("M-<up>" . sp-backward-up-sexp)
           ("C-M-f" . sp-forward-sexp)
           ("C-M-b" . sp-backward-sexp)
           ("C-M-n" . sp-next-sexp)
           ("C-M-p" . sp-previous-sexp)
           ("C-S-f" . sp-forward-symbol)
           ("C-S-b" . sp-backward-symbol)
           ("C-<right>" . sp-forward-slurp-sexp)
           ("M-<right>" . sp-forward-barf-sexp)
           ("C-<left>" . sp-backward-slurp-sexp)
           ("M-<left>" . sp-backward-barf-sexp)
           ("C-M-t" . sp-transpose-sexp)
           ("C-M-k" . sp-kill-sexp)
           ("C-k" . sp-kill-hybrid-sexp)
           ("M-k" . sp-backward-kill-sexp)
           ("C-M-w" . sp-copy-sexp)
           ("C-M-d" . delete-sexp)
           ("M-<backspace>" . backward-kill-word)
           ("C-<backspace>" . sp-backward-kill-word)
           ([remap sp-backward-kill-word] . backward-kill-word)
           ("M-[" . sp-backward-unwrap-sexp)
           ("M-]" . sp-unwrap-sexp)
           ("C-x C-t" . sp-transpose-hybrid-sexp)
           ("C-c (" . wrap-with-parens)
           ("C-c [" . wrap-with-brackets)
           ("C-c {" . wrap-with-braces)
           ("C-c '" . wrap-with-single-quotes)
           ("C-c \"" . wrap-with-double-quotes)
           ("C-c _" . wrap-with-underscores)
           ("C-c `" . wrap-with-back-quotes))
