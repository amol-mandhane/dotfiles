;;; init --- Base file for emacs config

;;; Commentary:
;;; Parse the config.org file and generate config.el

;;; Code:
(package-initialize)
(require 'ob-tangle)

(let
    ((enable-local-eval t)
     (inhibit-message t)
     (gc-cons-threshold 640000000))
  (org-babel-load-file "~/.emacs.d/config.org"))
(provide 'init)
;;; init.el ends here
