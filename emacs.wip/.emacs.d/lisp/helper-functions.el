;;; helper-functions --- General functions used across configs

;;; Commentary:
;;; This file contains some independent functions. Refer docstring for usage.

;;; Code:

(defun reload-config ()
  "Reload ~/.emacs.d/init.el"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun join-next-line ()
  "Join the next line to the current one."
  (interactive)
  (forward-line)
  (join-line))

(defmacro enable-minor-mode-globally (minor-mode)
  "Enables the minor mode globally.
MINOR-MODE: Symbol of minor mode to enable."
  `(progn
     (define-globalized-minor-mode
       ,(intern (concat "globalized-" (symbol-name minor-mode)))
       ,minor-mode
       (lambda () (,minor-mode +1)))
     (,(intern (concat "globalized-" (symbol-name minor-mode))) +1)))

(defun silence-function-message-advice (original &rest args)
  "Advice function for silencing message outputs of a function.
ORIGINAL: Advice argument.
ARGS: Advice argument."
  (let ((inhibit-message t)
	(message-log-max nil))
    (apply original args)))

(defmacro silence-function (func)
  "Silence the message outputs of the function for all executions in the future.
FUNC: Function to silence."
  `(progn (advice-add ,func :around #'silence-function-message-advice)))

(provide 'helper-functions)
;;; helper-functions.el ends here
