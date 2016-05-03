;;; emacs-config --- Configuration for Emacs editor

;;; Commentary:
;; Delegate the configuration work to emacs.org file.

;;; Code:

(package-initialize)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/emacs.org")
