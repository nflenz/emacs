;; Improve treesitter's syntax highlighting
(setq treesit-font-lock-level 4)

(use-package magit
  :custom
  ; Stop majit from splitting the window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (ryo-modal-keys
   ("SPC s" magit-file-dispatch)
   ("SPC t" magit-dispatch)))

(use-package diff-hl
  :init
;; Show uncommited changes in the buffer
  (global-diff-hl-mode)
  :config
  (ryo-modal-keys
   (" n" diff-hl-previous-hunk)
   ("SPC r i" diff-hl-next-hunk)
   ("SPC r r" diff-hl-revert-hunk)
   ("SPC r s" diff-hl-stage-dwim)))

;; Limit the vc backend to just git because VC slows down tramp
(setq vc-handled-backends '(Git))

(use-package smerge-mode
  :ensure nil
  :bind
  ("C-x C-v p" . smerge-prev)
  :config
  (ryo-modal-keys
   ("C-x C-v p" smerge-prev)
   ("C-x C-v n" smerge-next)
   ("C-x C-v u" smerge-keep-upper)
   ("C-x C-v l" smerge-keep-lower)))

;; Automatically use treesitter modes when available
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package paredit
  :hook
  (emacs-lisp-mode . paredit-mode)
  (lisp-interaction-mode . paredit-mode))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-interaction-mode . aggressive-indent-mode))

(use-package eglot
  :ensure nil)

(use-package flycheck)
(use-package consult-flycheck)
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package markdown-ts-mode
  :ensure t
  :after eglot
  :hook
  (markdown-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(markdown-ts-mode . ("marksman" "server"))))

(use-package powershell
  :after eglot
  :hook s
  (powershell-mode . electric-pair-local-mode)
  (powershell-mode . aggressive-indent-mode))

(use-package nix-ts-mode
  :after eglot
  :defer t
  :hook
  (nix-ts-mode . electric-pair-local-mode)
  (nix-ts-mode . aggressive-indent-mode)
  (eglot-ensure))

(use-package yaml-pro
  :defer t)
(use-package yaml-ts-mode
  :after eglot
  :ensure nil
  :hook
  (yaml-ts-mode . yaml-pro-mode)
  (yaml-ts-mode . electric-pair-local-mode)
  (yaml-ts-mode . yaml-pro-mode))

(use-package python-ts-mode
  :ensure nil
  :hook
  (python-ts-mode . electric-pair-local-mode))

(use-package systemd)

(use-package nushell-ts-mode
  :hook
  (nushell-ts-mode . aggressive-indent-mode)
  (nushell-ts-mode . eglot-ensure))

(use-package bash-ts-mode
  :ensure nil
  :hook
  (bash-ts-mode . aggressive-indent-mode)
  (bash-ts-mode . eglot-ensure))
