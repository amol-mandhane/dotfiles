;;; init --- Base file for emacs config

;;; Commentary:
;;; Parse the config.org file and generate config.el

;;; Code:
(package-initialize)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/config.org")
(provide 'init)
;;; init.el ends here
