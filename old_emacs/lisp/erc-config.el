;;; erc-config --- Configuration for ERC.

;;; Commentary:
;;;
;;; Module to configure and autoconnect to ERC.
;;; Isolated to separate file as this is a lot of code.

;;; Code:
(defvar erc-autoconnect-servers-list '(("irc.freenode.net" 6697 nil)
                                       ;;("127.0.0.1" 6668 nil)
                                       )
  "IRC servers to connect to.")

(defcustom erc-foolish-content '("^<.*?> \\?"
                                 "^<.*?> , *rr"
                                 "\\*CLICK\\*"
                                 "\\*BANG\\*"
                                 "^<.*?> !bonus")
  "Regular expressions to identify foolish content.
Usually what happens is that you add the bots to
`erc-ignore-list' and the bot commands to this list."
  :group 'erc
  :type '(repeat regexp))

(use-package erc
  :commands (erc erc-tls)
  :config
  (progn
    (setq erc-server-coding-system '(utf-8 . utf-8)
          erc-hide-list '("JOIN" "PART" "QUIT" "NICK")

          erc-rename-buffers t
          erc-join-buffer 'bury
          erc-kill-buffer-on-part t
          erc-kill-queries-on-quit t
          erc-kill-server-buffer-on-quit t

          erc-input-line-position -2)

    (defun erc-foolish-content (msg)
      "Check whether MSG is foolish."
      (erc-list-match erc-foolish-content msg))

    (add-hook 'erc-insert-pre-hook
              #'(lambda (s)
                  (when (erc-foolish-content s)
                    (setq erc-insert-this nil))))

    (add-hook 'kill-emacs-hook #'(lambda () (erc-cmd-GQUIT "")))))

(defmacro use-erc-module (module &rest args)
  "Use-package wrapper for loading erc MODULE with additional ARGS."
  (declare (indent 1))
  `(use-package ,(intern (concat "erc-" (symbol-name module)))
     :hook (erc-mode . ,(intern (concat "erc-" (symbol-name module) "-mode")))
     ,@args))

;; Set away status automatically
(use-erc-module autoaway
  :config
  (setq erc-auto-discard-away t
        erc-autoaway-idle-seconds 600
        erc-autoaway-message "AFK"
        erc-autoaway-idle-method 'irc))

;; Make links into clickable buttons
(use-erc-module button)

;; Complete nicknames and commands
(use-erc-module completion)

;; Wrap long lines and align messages
(use-erc-module fill
  :config
  (setq erc-fill-function #'erc-fill-static
        erc-fill-static-center 22))

;; Show images
(use-erc-module image
  :ensure t
  :commands (erc-image-show-url erc-image-mode))

;; /list support
(use-erc-module list)

;; Highlight pals, fools etc.
(use-erc-module match)

;; Display menu in ERC buffers
(use-erc-module menu)

(use-package erc-goodies
  ;; Move to prompt automatically
  :hook (erc-mode . erc-move-to-prompt-mode)
  ;; Don't show noncommands after eval
  :hook (erc-mode . erc-noncommands-mode)
  ;; Make displayed lines readonly
  :hook (erc-mode . erc-readonly-mode)
  ;; Scroll to bottom. Unfortunately this is outdated.
  ;; :hook (erc-mode . erc-scrolltobottom-mode)
  ;; Convert smileys to icons
  :hook (erc-mode . erc-smiley-mode))

;; Detect netsplits
(use-erc-module netsplit)

(use-erc-module networks)

;; Process CTCP PAGE requests
(use-erc-module page)

;; NickServ ID
(use-erc-module services
  :config
  (progn
    (setq erc-nickserv-identify-mode 'autodetect)
    ;; This is a hacky way to manage NickServ passwords. If erc-services impl
    ;; ever changes, update this.
    (defun erc--search-nickserv-password (nick)
      "Get nickserv password for NICK from auth-source."
      (let
          ((authdata
            (car
             (auth-source-search
              :host "nickserv.password"
              :port 1234
              :user nick))))
        (erc-nickserv-identify (funcall (plist-get authdata :secret)))))
    (advice-add
     'erc-nickserv-call-identify-function
     :override
     #'erc--search-nickserv-password)))

;; Check spellings
(use-erc-module spelling)

;; Show timestamps in ERC buffers
(use-erc-module stamp
  :config
  (setq erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "%H:%M:%S "
        erc-insert-timestamp-function 'erc-insert-timestamp-left))

;; Track channel activity in mode-line
(use-erc-module track
  :config
  (progn
    (defun reset-erc-track-mode ()
      "Reset erc-track details from modeline."
      (interactive)
      (setq erc-modified-channels-alist nil)
      (erc-modified-channels-update)
      (erc-modified-channels-display)
      (force-mode-line-update))

    (setq erc-track-exclude-types
          '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))))

;; Auto-join and NickServ identification cause race-conditions. Force
;; auto-join after identification.
(use-package erc-join
  :demand t
  :after erc-services
  :config
  (progn
    (setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#xmonad" "##machinelearning" "#chess")
            ("127.0.0.1" "#test")))
    (defun autojoin-after-nickserv-identification (proc parsed)
      "Autojoin after identification with NickServ."
      (with-current-buffer (process-buffer proc)
        (when (string-match ".*now identified for.*"
                            (erc-response.contents parsed))
          (erc-autojoin-channels erc-session-server (erc-current-nick))
          nil)))
    (add-hook 'erc-server-NOTICE-functions #'autojoin-after-nickserv-identification)))

;; Highlight nicks in different colors
(use-erc-module hl-nicks
  :ensure t)

;; Colorize ERC buffers.
(use-erc-module colorize
  :ensure t)

(defun erc-autoconnect-servers ()
  "Connect to `erc-autoconnect-servers-list'."
  (interactive)
  (cl-mapcar
   #'(lambda
       (spec)
       (let*
           ((inhibit-message t)
            (server (car spec))
            (port (cadr spec))
            (requires-auth (caddr spec))
            (authdata
             (car (auth-source-search :host server :port port))))
         (if requires-auth
             (erc-tls
              :server server
              :port port
              :nick (plist-get authdata :user)
              :password (funcall (plist-get authdata :secret)))
           (erc-tls
            :server server
            :port port
            :nick (plist-get authdata :user)))))
   erc-autoconnect-servers-list)
  (switch-to-buffer "*scratch*"))

(provide 'erc-config)
;;; erc-config.el ends here
