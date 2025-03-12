;;; config-lang-bazel.el --- Bazel configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)
(require 'use-package)

(use-package bazel
  :straight t
  :mode ((rx ?/ (or "BUILD" "BUILD.bazel") eos) . bazel-build-mode)
  :mode ((rx ?/ (or "WORKSPACE" "WORKSPACE.bazel" "WORKSPACE.bzlmod")
             eos) . bazel-workspace-mode)
  :mode ((rx "/MODULE.bazel" eos) . bazel-module-mode)
  :mode ((rx ?/ (+ nonl) ".bzl" eos) . bazel-starlark-mode)
  :mode ((rx ?/ (or "bazel.bazelrc" ".bazelrc") eos) . bazelrc-mode)
  :mode ((rx "/.bazelignore" eos) . bazelignore-mode)
  :mode ((rx "/.bazeliskrc" eos) . bazeliskrc-mode)
  :config
  (setq bazel-command "bazelisk"))

(provide 'config-lang-bazel)
;;; config-lang-bazel.el ends here
