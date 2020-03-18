;;; config-straight.el --- Straight.el setup         -*- lexical-binding: t; -*-


;;; Commentary:

;;; Code:

(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-recipe-repositories '(org-elpa melpa gnu-elpa-mirror))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq
 straight-default-files-directive
 '("[!.]*" "[!.]*/[!.]*" "[!.]*/[!.]*/[!.]*"
   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el")))

(use-package f
  :straight t
  :demand t)
(use-package s
  :straight t
  :demand t)
(use-package dash
  :straight t
  :demand t)


(provide 'config-straight)
;;; config-straight.el ends here
