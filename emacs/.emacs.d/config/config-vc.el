;;; config-vc.el --- Version Control                 -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(require 'straight)
(require 'use-package)
(require 'hydra)
(require 'helper-lib)

(setq make-backup-files nil
      version-control nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package ediff
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package magit
  :straight t
  :prefixed-bind (("gs" . magit-status)))

(use-package diff-hl
  :straight t
  :after (hydra)
  :hook (find-file . diff-hl-mode)
  :hook (find-file . diff-hl-flydiff-mode)
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (defhydra diff-hl-hydra (:foreign-keys warn)
    "diff-hl-hydra"
    ("n" diff-hl-next-hunk "Next Hunk")
    ("p" diff-hl-previous-hunk "Previous Hunk")
    ("k" diff-hl-revert-hunk "Kill Hunk")
    ("q" nil "Quit")))

(use-package smerge-mode
  :after (hydra)
  :prefixed-bind (:map smerge-mode-map
                       ("m" . hydra-smerge/body))
  :config
  (eval-when-compile
    (unless (>= emacs-major-version 26)
      (defalias 'smerge-keep-upper 'smerge-keep-mine)
      (defalias 'smerge-keep-lower 'smerge-keep-other)
      (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
      (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
      (defalias 'smerge-diff-base-lower 'smerge-diff-base-other)))
  (defhydra hydra-smerge
    (:foreign-keys warn)
    "
^Move^	^Keep^	^Aux^	^Diff^
------------------------------------------------------
_n_ext	_b_ase	_R_efine	_<_: base-upper	_q_uit
_p_rev	_u_pper	_E_diff	_=_: upper-lower	_RET_: current
^ ^	_l_ower	_C_ombine	_>_: base-lower
^ ^	_a_ll	_r_esolve"
    ("RET" smerge-keep-current)
    ("C" smerge-combine-with-next)
    ("E" smerge-ediff)
    ("R" smerge-refine)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("n" smerge-next)
    ("l" smerge-keep-lower)
    ("p" smerge-prev)
    ("r" smerge-resolve)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("q" nil :color red)))

(provide 'config-vc)
;;; config-vc.el ends here
