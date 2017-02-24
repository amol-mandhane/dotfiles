;; Module for various line numbering functions

(require 'linum)

(defvar linum-current-line 1 "Current line number.")
(defvar linum-format-string "%5d \u2502" "String for formatting line numbers")

;; Define styles for line numbers
(custom-set-faces '(linum ((t (:background "black" :foreground "#666462")))))

(defface linum-current-line
  `((t :inherit linum
       :background "goldenrod"
       :foreground "black"
       :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)))

(defun linum-absolute-line-numbers (line-number)
  "Display absolute line numbers with current line number highlighted."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                  'linum)))
    (propertize (format linum-format-string line-number)
                'face face)))

(defun linum-relative-line-numbers (line-number)
  "Display relative line numbers with current line number displayed and highlighted."
  (let ((offset (- line-number linum-current-line)))
    (if (= offset 0)
      (propertize (format linum-format-string line-number) 'face 'linum-current-line)
      (propertize (format linum-format-string (abs offset)) 'face 'linum))))

(global-linum-mode t)
(setq linum-format 'linum-relative-line-numbers)

(provide 'line-numbering)
