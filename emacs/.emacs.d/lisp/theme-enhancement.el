;;; theme-enhancement -- Script for enhancing color themes.

;;; Commentary:
;;;
;;; theme-enhancement library provides a function to enhance theme by making
;;; certain faces bold and italic. The faces to make bold are borrowed from
;;; `spacemacs-dark' theme.
;;;
;;; Usage:
;;;
;;;   (theme-enhancement/apply)

;;; Code:

(defconst +theme-enhancement/bold-faces+
  '(bold
    bold-italic
    buffer-menu-buffer
    comint-highlight-input
    compilation-mode-line-exit
    compilation-mode-line-fail
    compilation-mode-line-run
    custom-group-subtitle
    custom-group-tag
    custom-group-tag-1
    custom-variable-button
    custom-variable-tag
    diff-file-header
    epa-field-name
    epa-mark
    epa-validity-high
    escape-glyph
    flx-highlight-face
    flycheck-fringe-error
    flycheck-fringe-info
    flycheck-fringe-warning
    font-lock-builtin-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-warning-face
    git-commit-comment-action
    git-commit-comment-branch
    git-commit-comment-heading
    ido-first-match
    ido-only-match
    info-menu-header
    isearch
    link
    magit-blame-summary
    magit-branch-current
    magit-branch-local
    magit-branch-remote
    magit-diff-file-heading
    magit-diff-file-heading-highlight
    magit-diff-file-heading-selection
    magit-head
    magit-popup-argument
    magit-popup-heading
    magit-popup-key
    magit-popup-option-value
    magit-process-ng
    magit-process-ok
    magit-refname
    magit-refname-stash
    magit-refname-wip
    magit-section-heading
    magit-section-heading-selection
    magit-section-secondary-heading
    magit-tag
    match
    message-header-cc
    message-header-newsgroups
    message-header-subject
    message-header-to
    message-mml
    mode-line-buffer-id
    mode-line-emphasis
    monky-branch
    monky-log-date
    monky-queue-patch
    monky-section-title
    org-agenda-date-today
    org-archived
    org-column-title
    org-done
    org-list-dt
    org-tag
    org-todo
    org-warning
    read-multiple-choice-face
    show-paren-match
    show-paren-mismatch
    sp-show-pair-match-face
    sp-show-pair-mismatch-face
    success
    tty-menu-enabled-face
    warning
    widget-button))

(defconst +theme-enhancement/italic-faces+
  '(font-lock-comment-face
    font-lock-doc-face))

(require 'org)
(defun theme-enhancement--org-mode ()
  "Apply theme enhancements to `org-mode' faces.

Currently these are borrowed from Cyberpunk theme."
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
  (set-face-attribute 'org-document-info nil :weight 'bold)
  (set-face-attribute 'org-agenda-date-today nil :slant 'italic :weight 'bold)
  (set-face-attribute 'org-agenda-structure nil :inherit font-lock-comment-face)
  (set-face-attribute 'org-archived nil :slant 'italic)
  (set-face-attribute 'org-checkbox nil :box '(:line-width 1 :style 'released-button))
  (set-face-attribute 'org-date nil :underline t)
  (set-face-attribute 'org-done nil :bold t :weight 'bold :box '(:line-width 1 :style 'none))
  (set-face-attribute 'org-todo nil :bold t :weight 'bold :box '(:line-width 1 :style 'none))
  (set-face-attribute 'org-level-1 nil :bold t :weight 'bold :height 1.2)
  (set-face-attribute 'org-level-2 nil :height 1.1)
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-tag nil :bold t :weight 'bold)
  (set-face-attribute 'org-column-title nil :underline t :weight 'bold))

(require 'cl-lib)

(defun theme-enhancement/apply ()
  "Apply theme enhancements."
  (progn
    (cl-loop for f in +theme-enhancement/bold-faces+
             collect (ignore-errors (set-face-bold f t)))
    (cl-loop for f in +theme-enhancement/italic-faces+
             collect (set-face-italic f t))
    (theme-enhancement--org-mode)))

(provide 'theme-enhancement)
;;; theme-enhancement.el ends here
