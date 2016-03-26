;; Indentation setup using 2 spaces

(defvar indent-spaces 2 "Number of spaces for the indentation.")

(setq-default indent-tabs-mode nil)
(setq-default tab-width indent-spaces)
  ;; java/c/c++
(setq c-basic-offset indent-spaces)
(setq python-indent indent-spaces)
(setq coffee-tab-width indent-spaces) ; coffeescript
(setq javascript-indent-level indent-spaces) ; javascript-mode
(setq js-indent-level indent-spaces) ; js-mode
(setq js2-basic-offset indent-spaces) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
(setq web-mode-markup-indent-offset indent-spaces) ; web-mode, html tag in html file
(setq web-mode-css-indent-offset indent-spaces) ; web-mode, css in html file
(setq web-mode-code-indent-offset indent-spaces) ; web-mode, js code in html file
(setq css-indent-offset indent-spaces) ; css-mode


(provide 'indentation)
