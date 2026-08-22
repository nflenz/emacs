;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-x C-r") #'vc-restore)

;; Improve treesitter's syntax highlighting
(setq treesit-font-lock-level 4)

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package smerge-mode
  :ensure nil
  :bind
  (:map smerge-mode-map
	("M-n" . smerge-next)
	("M-p" . smerge-prev)
	("M-u" . smerge-keep-upper)
	("M-l" . smerge-keep-lower)))

(use-package yasnippet-snippets)

(use-package magit
  :custom
  ;; Stop magit from splitting the window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :custom
  (diff-hl-show-staged-changes nil)
  :init
  ;; Show uncommited changes in the buffer
  (global-diff-hl-mode))

;; Limit the vc backend to just git because VC slows down tramp
(setq vc-handled-backends '(Git))

(use-package reformatter)

;; Automatically use treesitter modes when available
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (use-package paredit
;;   :hook
;;   (emacs-lisp-mode . paredit-mode)
;;   (lisp-interaction-mode . paredit-mode))


(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions
			(list (cape-capf-super #'yasnippet-capf #'elisp-completion-at-point)))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions
			(list (cape-capf-super #'yasnippet-capf #'elisp-completion-at-point)))))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-interaction-mode . aggressive-indent-mode))

(use-package eglot
  :ensure nil)

(use-package flycheck)
(use-package consult-flycheck
  :bind
  ("C-c e" . consult-flycheck))

(use-package sideline
  :hook
  (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flycheck)))

(use-package sideline-flycheck
  :hook
  (flycheck-mode . sideline-flycheck-setup))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package dockerfile-ts-mode
  :ensure nil
  :hook
  (dockerfile-ts-mode . eglot-ensure))

(use-package markdown-ts-mode
  :defer nil
  :ensure nil
  :hook
  (markdown-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(markdown-ts-mode . ("marksman" "server"))))

(use-package powershell
  :hook
  (powershell-mode . electric-pair-local-mode)
  (powershell-mode . eglot-ensure)
  (powershell-mode . aggressive-indent-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '(powershell-mode . ("powershell-editor-services" "-Stdio"))))

(use-package nix-ts-mode
  :defer t
  :hook
  (nix-ts-mode . electric-pair-local-mode)
  (nix-ts-mode . aggressive-indent-mode)
  (nix-ts-mode . eglot-ensure))

(use-package yaml-pro
  :bind
  (:map yaml-pro-ts-mode-map
	("C-M-p" . yaml-pro-ts-prev-subtree)
	("C-M-n" . yaml-pro-ts-next-subtree)
	("C-M-k" . yaml-pro-ts-kill-subtree)
	("C-M-u" . yaml-pro-ts-up-level)
	("C-M-d" . yaml-pro-ts-down-level)
	("C-M-a" . yaml-pro-ts-first-sibling)
	("C-M-e" . yaml-pro-ts-last-sibling)
	("C-M-t" . yaml-pro-ts-move-subtree-down)
	("C-M-T" . yaml-pro-ts-move-subtree-up)
	("C-c <" . yaml-pro-ts-unindent-subtree)
	("C-c >" . yaml-pro-ts-indent-subtree)
	("C-c C-s" . yaml-pro-jump)))

(use-package yaml-ts-mode
  :ensure nil
  :hook
  (yaml-ts-mode . eglot-ensure)
  (yaml-ts-mode . yaml-pro-ts-mode)
  (yaml-ts-mode . electric-pair-local-mode))

(use-package python-ts-mode
  :ensure nil
  :hook
  (python-ts-mode . electric-pair-local-mode)
  (python-ts-mode . eglot-ensure)
  :config
  ;; Remove rass from the configuration
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs nil nil #'equal)
        '("basedpyright-langserver" "--stdio"))
  (add-to-list 'eglot-server-programs
	       '(python-ts-mode . ("basedpyright-langserver" "--stdio"))))

(use-package systemd
  :hook
  (systemd-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(systemd-mode . ("systemd-language-server"))))

(use-package nushell-ts-mode
  :hook
  (nushell-ts-mode . aggressive-indent-mode)
  (nushell-ts-mode . eglot-ensure))

(use-package bash-ts-mode
  :ensure nil
  :hook
  (bash-ts-mode . aggressive-indent-mode)
  (bash-ts-mode . eglot-ensure))

(use-package just-ts-mode
  :hook
  (just-ts-mode . eglot-ensure)
  (just-ts-mode . aggressive-indent-mode)
  :config
  (add-to-list 'eglot-server-programs
               '(just-ts-mode . ("just-lsp"))))

(use-package terraform-mode
  :hook
  (terraform-mode . eglot-ensure)
  (terraform-mode . aggressive-indent-mode))

(use-package awk-ts-mode
  :ensure nil
  :hook
  (awk-ts-mode . eglot-ensure)
  (awk-ts-mode . aggressive-indent-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '(awk-ts-mode . ("awk-language-server"))))

(use-package lua-ts-mode
  :ensure nil
  :hook
  (lua-ts-mode . eglot-ensure)
  (lua-ts-mode . aggressive-indent-mode))

(use-package js-ts-mode
  :ensure nil
  :hook
  (js-ts-mode . eglot-ensure)
  (js-ts-mode . aggressive-indent-mode))

(use-package typescript-ts-mode
  :ensure nil
  :hook
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . aggressive-indent-mode))

(use-package java-ts-mode
  :ensure nil
  :hook
  (java-ts-mode . eglot-ensure)
  (java-ts-mode . aggressive-indent-mode))

(use-package rust-ts-mode
  :ensure nil
  :hook
  (rust-ts-mode . eglot-ensure)
  (rust-ts-mdoe . aggressive-indent-mode))

(use-package ruby-ts-mode
  :ensure nil
  :hook
  (ruby-ts-mode . eglot-ensure)
  (ruby-ts-mode . aggressive-indent-mode))

(use-package elixir-ts-mode
  :hook
  (elixir-ts-mode . eglot-ensure)
  (elixir-ts-mode . aggressive-indent-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '(elixir-ts-mode . ("elixir-ls"))))

(use-package jq-ts-mode
  :hook
  (jq-ts-mode . eglot-ensure)
  (jq-ts-mode . aggressive-indent-mode))

(use-package perl-ts-mode
  :ensure nil
  :hook
  (perl-mode . eglot-ensure)
  (perl-mode . aggressive-indent-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '(perl-mode . ("perlnavigator"))))

(use-package zig-ts-mode
  :hook
  (zig-ts-mode . eglot-ensure)
  (zig-ts-mode . aggressive-indent-mode))

(use-package haskell-ts-mode
  :hook
  (haskell-ts-mode . eglot-ensure)
  (haskell-ts-mode . aggressive-indent-mode))

(use-package go-ts-mode
  :ensure nil
  :hook
  (go-ts-mode . eglot-ensure)
  (go-ts-mode . aggressive-indent-mode)
  (go-ts-mode . go-format-on-save-mode)
  :config
  (reformatter-define go-format
    :program "gofmt"))

(use-package sql-mode
  :ensure nil
  :hook
  (sql-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(sql-mode . '("sqls"))))
