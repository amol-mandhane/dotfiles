;;; helium-modeline --- Custom modeline for Emacs
;;;
;;; Commentary:
;;;
;;; Helium modeline is a custom modeline based on powerline framework.
;;; It is inspired from Doom themes which are inspired from Atom.
;;;
;;; Usage:
;;;   (require 'helium-mode-line)
;;;   (powerline-helium-theme)
;;;
;;; Customization:
;;;   To customize colors, Run M-x customize-group RET helium-mode-line RET.
;;;
;;;   Currently, there is no API for customization of mode-line segments.
;;;   Edit source directly.
;;;
;;; Colors:
;;;   Currently, helium-mode-line pulls colors from `font-lock-keyword-face',
;;;   `font-lock-function-name-face' and `error-face' for mode-line colors.
;;;   This will be customized into a light-theme and a dark-theme later. For
;;;   light theme, colors will be pulled from Adwaita theme.
;;;   Dark theme base isn't finalized yet.
;;;
;;; Code:

(require 'powerline)
(require 'projectile)
(require 'flycheck)
(require 'let-alist)
(require 'all-the-icons)

(defgroup helium-mode-line nil
  "Group for the faces of Helium mode line."
  :group 'mode-line)

(defface helium-mode-line-default
  '((t (:inherit mode-line)))
  "Default face for the Helium mode line."
  :group 'helium-mode-line)

(defface helium-mode-line-inactive
  '((t (:inherit mode-line-inactive)))
    "Face for inactive Helium mode line."
    :group 'helium-mode-line)

(defface helium-mode-line-inactive-bold
  '((t (:inherit helium-mode-line-inactive :weight bold :bold t)))
  "Default face for the Helium mode line."
  :group 'helium-mode-line)

(defface helium-mode-line-colored
  '((t (:inherit helium-mode-line-default)))
  "Face for colored text of the Helium mode line."
  :group 'helium-mode-line)

(defface helium-mode-line-colored-bold
  '((t (:weight bold :inherit helium-mode-line-colored)))
  "Face for bold colored text of the Helium mode line."
  :group 'helium-mode-line)

(defface helium-mode-line-color-1
  '((t (:inherit font-lock-function-name-face)))
  "First color of the helium modeline colors."
  :group 'helium-mode-line)

(defface helium-mode-line-color-2
  '((t (:inherit font-lock-keyword-face)))
  "Second color of the helium modeline colors."
  :group 'helium-mode-line)

(defface helium-mode-line-active-block
  '((t (:inherit helium-mode-line-default :weight bold)))
  "Face for the left side block of the mode-line when mode-line is active."
  :group 'helium-mode-line)

(defface helium-mode-line-inactive-block
  '((t (:inherit helium-mode-line-default :weight bold)))
  "Face for the left side block of the mode-line when mode-line is inactive."
  :group 'helium-mode-line)

(defface helium-mode-line-errors
  '((t (:inherit error)))
  "Face for the errors display in mode-line."
  :group 'helium-mode-line)

(defface helium-mode-line-no-errors
  '((t (:inherit helium-mode-line-errors :foreground "green")))
  "Face for the errors display in mode-line when there are no errors."
  :group 'helium-mode-line)

(defvar helium-mode-line-height 28 "Height of the mode-line.")
(defvar helium-mode-line-block-width 1 "Width of the left XPM block on the mode-line.")

(defun helium--project-root-for-prefix-strip ()
  "Get the path to the root of the project."
  (condition-case nil (projectile-project-root) (error "")))

(defun helium--simplify-buffer-name-prefix (buffer-name-prefix)
  "Compress the buffer name prefix if it is too long.

BUFFER-NAME-PREFIX: Intended prefix of the buffer name."
  (let
      ((threshold (- 40 (length (buffer-name)))))
    (if (<= threshold (length buffer-name-prefix))
        (let
            ((filtered-tokens
              (cl-remove-if
               #'(lambda (s) (string= s ""))
               (split-string buffer-name-prefix "/"))))
          (concat "/"
                  (cl-reduce
                   #'(lambda (s acc) (if (<= threshold (length acc))
                                         acc
                                       (concat s "/" acc)))
                   filtered-tokens :initial-value "" :from-end t)))
      buffer-name-prefix)))

(defun helium--buffer-name-prefix ()
  "Get the prefix path to the buffer name.

If the buffer is a file and part of the project, the prefix is
relative path to the directory from the project root.
Empty string otherwise."
  (let ((buffer-full-path (buffer-file-name))
        (buffer-project-root (helium--project-root-for-prefix-strip)))
    (if (not buffer-full-path)
        ""
      (or (helium--simplify-buffer-name-prefix
           (file-name-directory
            (string-remove-prefix buffer-project-root buffer-full-path))) ""))))

(defun helium--project-name ()
  "Name of the project of current buffer."
  (let
      ((project-name (projectile-project-name)))
    (if (eq project-name "-") "" (concat "[" project-name "]"))))

(defun helium--errors (error-face no-error-face)
  "Generate the string to display errors in the mode-line.

Also propertizes the string to show flycheck mode menu on clicking the
mode-line section.

ERROR-FACE: Face to use to display error information.
NO-ERROR-FACE: Face to use to display \"no errors\" information."
  (when (boundp 'flycheck-last-status-change)
    (let*
        ((all-the-icons-default-adjust 0)
         (all-the-icons-scale-factor 1)
         (flycheck-string
          (pcase flycheck-last-status-change
            ('finished
             (if flycheck-current-errors
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (concat
                    (all-the-icons-octicon "flame" :face error-face)
                    " "
                    (prin1-to-string (or .error 0))
                    " "
                    (all-the-icons-octicon "alert" :face error-face)
                    " "
                    (prin1-to-string (or .warning 0))))
               (all-the-icons-octicon "check" :face no-error-face)))
            ('running (all-the-icons-octicon "beaker" :face error-face))
            ('no-checker (all-the-icons-octicon "circle-slash" :face error-face))
            ('errored (all-the-icons-octicon "alert" :face error-face))
            ('interrupted (all-the-icons-octicon "x" :face error-face))
            (default (all-the-icons-octicon "x" :face error-face)))))
      (propertize
       flycheck-string
       'mouse-face 'mode-line-highlight
       'help-echo "Flycheck mode\n\nmouse-1: Flycheck-mode help menu"
       'local-map
       (let
           ((map (make-sparse-keymap)))
         (define-key map [mode-line down-mouse-1] flycheck-mode-menu-map)
         map)))))

(defun helium--make-block-xpm (&rest args)
  "Create an XPM bitmap for left side block.

At this point, the left block is used just to give the mode-line its height.

ARGS: Unused arguments."
  (let ((height (symbol-value 'helium-mode-line-height))
        (width (symbol-value 'helium-mode-line-block-width))
        (color (face-attribute 'helium-mode-line-colored :foreground)))
    (when window-system
      (propertize
       " " 'display
       (let ((data nil)
             (i 0))
         (setq data (make-list height (make-list width 1)))
         (pl/make-xpm "percent" color color (reverse data)))))))

(pl/memoize 'helium--make-block-xpm)

(defun helium--update-faces ()
  "Update the colors of the helium-modeline faces."
  (set-face-attribute 'mode-line nil :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :height 0.9)
  (set-face-attribute 'helium-mode-line-color-2 nil :height 0.9)
  (set-face-foreground 'helium-mode-line-colored (face-foreground 'helium-mode-line-color-1 nil 'default))
  (set-face-foreground 'helium-mode-line-colored-bold (face-foreground 'helium-mode-line-color-1 nil 'default))
  (set-face-foreground 'helium-mode-line-active-block (face-background 'helium-mode-line-color-1 nil 'default))
  (set-face-background 'helium-mode-line-active-block (face-foreground 'helium-mode-line-color-1 nil 'default))
  (set-face-foreground 'helium-mode-line-inactive-block (face-background 'helium-mode-line-default nil 'default))
  (set-face-background 'helium-mode-line-inactive-block (face-foreground 'helium-mode-line-default nil 'default)))

(defun helium--get-faces (active-p)
  "Get the faces to apply depending on whether the mode-line is active or not.

ACTIVE-P: Boolean representing whether the modeline is active or not."
  (if active-p
      '((default-face . helium-mode-line-default)
        (highlight-face . helium-mode-line-colored)
        (highlight-bold-face . helium-mode-line-colored-bold)
        (block-face . helium-mode-line-active-block)
        (vc-face . helium-mode-line-color-2)
        (error-face . helium-mode-line-errors)
        (no-error-face . helium-mode-line-no-errors))
    '((default-face . helium-mode-line-inactive)
      (highlight-face . helium-mode-line-inactive)
      (highlight-bold-face . helium-mode-line-inactive-bold)
      (block-face . helium-mode-line-inactive-block)
      (vc-face . helium-mode-line-inactive)
      (error-face . helium-mode-line-inactive)
      (no-error-face . helium-mode-line-inactive))))

(defun powerline-helium-theme ()
  "Helium theme for powerline."
  (interactive)
  (helium--update-faces)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let-alist (helium--get-faces (powerline-selected-window-active))
        (let*
            ((lhs
              (list
               (helium--make-block-xpm)
               (powerline-raw (concat " " (window-numbering-get-number-string)) .block-face 'r)
               (powerline-raw (if buffer-read-only "" "") .default-face 'l)
               (powerline-raw (if (buffer-modified-p) "" "") .default-face 'l)
               (powerline-raw (helium--project-name) .highlight-face 'l)
               (powerline-raw (helium--buffer-name-prefix) .default-face )
               (powerline-raw (buffer-name) .highlight-bold-face)
               (powerline-raw " %4l:%c " .default-face)))
             (center
              (list
               (powerline-raw (helium--errors .error-face .no-error-face) .error-face)
               (powerline-major-mode .highlight-face 'l)
               (powerline-vc .vc-face)))
             (rhs
              (list
               (powerline-minor-modes .default-face 'r)
               (powerline-raw mode-line-misc-info .error-face 'r)))
             (merged-rhs (append center
                                 (list (powerline-raw " " .default-face))
                                 rhs)))
          (concat
           (powerline-render lhs)
           ;; (powerline-fill-center .default-face (/ (powerline-width center) 2.0))
           ;; (powerline-render center)
           ;; (powerline-fill .default-face (+ (powerline-width rhs) 1))
           (powerline-fill .default-face (powerline-width merged-rhs))
           (powerline-render merged-rhs))))))))

(provide 'helium-modeline)
;;; helium-modeline.el ends here
