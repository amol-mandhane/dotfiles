;;; init --- Base file for emacs config

;;; Commentary:
;;; Parse the config.org file and generate config.el

;;; Code:
(let
    ((enable-local-eval t)
     (inhibit-message t)
     (ad-redefinition-action 'accept)
     (file-name-handler-alist nil)
     (load-prefer-newer t)
     (gc-cons-threshold 640000000))
  (load-file "~/.emacs.d/config.el"))
(provide 'init)
;;; init.el ends here
