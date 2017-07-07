(add-to-list 'load-path (expand-file-name "~/.spacemacs.d"))

(setq ns-use-srgb-colorspace nil)

(setq vi-tilde-fringe-bitmap-array [0 0 0 3 3 255 0 0])

(menu-bar-mode 1)

(setq require-final-newline t)

(setq custom-file "/dev/null")

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

(add-hook 'shell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook 'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'shell-mode-hook 'with-editor-export-git-editor)

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(require 'stickyfunc-enhance)

(semantic-mode 1)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))

(add-to-list 'flycheck-ghc-search-path (expand-file-name "~/.xmonad/lib"))

(setq org-agenda-files '("~/organizer"))

(setq org-capture-templates
      '(("a" "Action Item" entry (file+headline "~/organizer/main.org" "Action Items")
         "* TODO %?\n  %i")
        ("c" "Calendar" entry (file+headline "~/organizer/main.org" "Calendar")
         "* %?\n %^T\n %i")
        ("r" "Reference" entry (file "~/organizer/reference.org")
         "* %?\n  %i\n%^{prompt|Description}\n\n:PROPERTIES:\n:RecordDate:\t%T\n:END:"
         :prepend t
         :empty-lines 1)))

(evil-ex-define-cmd "W" "w")

(require 'smartparens)

(defmacro def-pair (pair)
  `(progn (defun ,(read (concat "sp/wrap-with-"
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

(defun setup-smartparens-keys ()
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
             ("C-)" . sp-forward-slurp-sexp)
             ("C-}" . sp-forward-barf-sexp)
             ("C-(" . sp-backward-slurp-sexp)
             ("C-{" . sp-backward-barf-sexp)
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
             ("M-(" . sp/wrap-with-parens)
             ("C-c (" . sp/wrap-with-parens)
             ("C-c [" . sp/wrap-with-brackets)
             ("C-c {" . sp/wrap-with-braces)
             ("C-c '" . sp/wrap-with-single-quotes)
             ("C-c \"" . sp/wrap-with-double-quotes)
             ("C-c _" . sp/wrap-with-underscores)
             ("C-c `" . sp/wrap-with-back-quotes)))
(setup-smartparens-keys)
