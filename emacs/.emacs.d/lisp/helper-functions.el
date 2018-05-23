;;; helper-functions --- General functions used across configs

;;; Commentary:
;;; This file contains some independent functions.  Refer docstring for usage.

;;; Code:
(require 'cl-lib)

(defun reload-config ()
  "Reload ~/.emacs.d/init.el."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun switch-to-previous-buffer (&optional window)
  "Switch back and forth between current and last buffer in the WINDOW."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun join-next-line ()
  "Join the next line to the current one."
  (interactive)
  (forward-line)
  (join-line))

(defmacro enable-minor-mode-globally (minor-mode-to-enable)
  "Enable the minor mode globally.
MINOR-MODE: Symbol of minor mode to enable."
  `(progn
     (define-globalized-minor-mode
       ,(intern (concat "globalized-" (symbol-name minor-mode-to-enable)))
       ,minor-mode-to-enable
       (lambda () (,minor-mode-to-enable +1)))
     (,(intern (concat "globalized-" (symbol-name minor-mode-to-enable))) +1)))

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

(defun byte-recompile-configuration ()
  "Byte-compile the configuration."
  (interactive)
  (org-babel-tangle-file "~/.emacs.d/config.org")
  (byte-recompile-file "~/.emacs.d/config.el" :force 0 nil)
  (byte-recompile-directory "~/.emacs.d/lisp/" 0 :force))

(provide 'helper-functions)
;;; helper-functions.el ends here
