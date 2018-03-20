;;; init --- Base file for emacs config

;;; Commentary:
;;; Parse the config.org file and generate config.el

;;; Code:
(package-initialize)
(require 'ob-tangle)

(let
    ((enable-local-eval t)
     (inhibit-message t)
     (ad-redefinition-action 'accept)
     (file-name-handler-alist nil)
     (load-prefer-newer t)
     (gc-cons-threshold 640000000))
  (if (file-newer-than-file-p "~/.emacs.d/config.org" "~/.emacs.d/config.el")
      (org-babel-load-file "~/.emacs.d/config.org")
    (load-file "~/.emacs.d/config.el")))
(provide 'init)
;;; init.el ends here
