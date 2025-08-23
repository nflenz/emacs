(setq conf-dir (file-name-parent-directory user-init-file))

(defun loadconf (args)
  "Load elisp in emacs configuration directory"
  (load (concat conf-dir args)))

(setq use-package-always-ensure t)

(loadconf "elpaca.el")
(loadconf "transpositions.el")
(loadconf "evil.el")

;; Stop emacs from modifying init.el
(load (setq custom-file (concat conf-dir "custom-set-variables.el")))

;; DISABLE the startup help screen
(setq inhibit-startup-message t)

;; Disable those annoying byte compile warnings
(setq byte-compile-warnings nil)

;; Unwanted GUI features
(progn
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; The system bell is completely useless and annoying. No thank you.
(setq ring-bell-function 'ignore)

;; Disable backups and autosaves
(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package dired
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t)
  :hook
  (dired . auto-revert-mode))

;; Show available keybindings
(use-package which-key
  :config
  (which-key-mode 1))

;; Transparency and font size
(progn
  (set-frame-parameter nil 'alpha-background 95)
  (add-to-list 'default-frame-alist '(alpha-background . 95))
  (set-face-attribute 'default nil :height 130))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-1337))

;; Always use y/n for confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package embark-consult)
(use-package embark
  :config
  (evil-define-key '(normal motion) global-map "z" #'embark-act)
  (evil-define-key '(normal insert) global-map (kbd "C-b") #'embark-bindings))

;; Minibuffer completions
(use-package vertico
  :config
  (vertico-mode 1)  
  (evil-define-key 'normal vertico-map "u" #'vertico-previous)
  (evil-define-key 'normal vertico-map "e" #'vertico-next)
  (evil-define-key 'normal vertico-map (kbd "RET") #'vertico-exit)
  (evil-define-key 'normal vertico-map (kbd "<tab>") #'vertico-insert))

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay 0)
  (corfu-count 30)
  (corfu-auto-prefix 0)
  :bind
  ("C-<tab>" . completion-at-point)
  :config
  (add-to-list 'completion-category-overrides '(lsp-capf (styples basic substring)))
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (keymap-unset corfu-map "RET")

  ;; Corfu doesn't seem to work well with evil-mode
  (defun my/tab (args)
    "first try yas-expand
     second try yas-next-field
     thrid try corfu-complete
     else indent"
    (interactive "P")
    (if (not (condition-case err
		 (yas-expand)
	       (error nil)))
	(if (not (condition-case err
		     (yas-next-field)
		   (error nil)))
	    (if (not (condition-case err
			 (corfu-complete)
		       (error nil)))
		(indent-for-tab-command)))))  

  (evil-define-key '(normal) corfu-map (kbd "<tab>") #'my/tab)
  (evil-define-key '(insert) global-map (kbd "<tab>") #'my/tab)
  
  (evil-define-key '(normal) corfu-map (kbd "M-e") #'corfu-next)
  (evil-define-key '(normal) corfu-map (kbd "M-u") #'corfu-previous)
  (evil-define-key '(insert) global-map (kbd "M-e") #'corfu-next)
  (evil-define-key '(insert) globbal-map (kbd "M-u") #'corfu-previous)
  
  (evil-define-key '(normal) corfu-map "e" (lambda (args)
					     (interactive "P")
					     (corfu-quit)
					     (next-line args)))
  (evil-define-key '(normal) corfu-map "u" (lambda (args)
					     (interactive "P")
					     (corfu-quit)
					     (previous-line args))))

;; Search for candidates without typing strings in order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Display completion candidates in order
(use-package prescient
  :demand t
  :config
  (use-package vertico-prescient
    :demand t
    :config
    (vertico-prescient-mode 1))
  (use-package corfu-prescient
    :demand t
    :config
    (corfu-prescient-mode 1)))

;; Extra information for completion candidates
(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :bind
  (("C-s" . #'consult-line)))

;; Better help commands
(use-package helpful
  :bind
  (("C-h f" . 'helpful-callable)
   ("C-h v" . 'helpful-variable)
   ("C-h k" . 'helpful-key)
   ("C-h x" . 'helpful-command)))

(use-package elfeed
  :custom
  (elfeed-feeds '("https://discourse.nixos.org/c/links.rss"))
  :config
  (evil-set-initial-state #'elfeed-search-mode 'emacs))

;; Terminal emulation inside emacs
(use-package vterm)

;; Automatic indentation for source code
(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package repl-driven-development)

(use-package paredit
  :hook
  (emacs-lisp-mode . paredit-mode))

(use-package yasnippet-snippets)
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package transient)
(use-package magit
  :config
  (evil-define-key '(normal insert) global-map (kbd "M-p") #'magit-file-dispatch)
  (evil-define-key '(normal insert) global-map (kbd "M-f") #'magit-dispatch)
  (evil-define-key '(normal insert) global-map (kbd "M-w") #'projectile-vc)
  (evil-set-initial-state #'magit-status-mode 'emacs))

;; Language server protocol
(use-package spinner)
(use-package lsp-mode
  :demand t
  :bind-keymap
  ("<f5>" . lsp-command-map)
  :custom
  ((lsp-keymap-prefix "M-p")
   (lsp-enable-suggest-server-download nil)
   (gc-cons-threshold 100000000)
   (read-process-output-max (* 1024 1024)))
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (lsp-enable-which-key-integration t)
  (evil-define-key '(normal insert) global-map (kbd lsp-keymap-prefix) lsp-command-map)
  
  (if (getenv "http_proxy")
      (setq lsp-http-proxy (concat "http://" (getenv "http_proxy"))))

  ;; Use the lsp-booster for better performance
  (loadconf "lsp-booster.el"))

(use-package lsp-ui
  :after lsp-mode
  :custom
  ((lsp-ui-sideline-show-hover t)
   (lsp-ui-sideline-show-code-actions t)
   (lsp-ui-sideline-update-mode 'line)
   (lsp-ui-sideline-delay 0)
   (lsp-ui-doc-delay 0))
  :hook
  (lsp-mode . lsp-ui-mode))

;; Error checking
(use-package flycheck
  :hook
  (lsp-mode . flycheck-mode))

(use-package consult-flycheck
  :config
  (evil-define-key '(normal visual motion insert) global-map (kbd "C-e") #'consult-flycheck))

(use-package dape)

(use-package projectile
  :custom
  (projectile-project-search-path '("~/src/"))
  :config
  (projectile-discover-projects-in-search-path))

(use-package nix-mode
  :hook
  (nix-mode . aggressive-indent-mode)
  (nix-mode . lsp-deferred))

(use-package sh-mode
  :ensure nil
  :hook
  (sh-mode . lsp-deferred))

(use-package powershell
  :hook
  (powershell-mode . lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("powershell-editor-services -Stdio"))
    :major-modes '(powershell-mode)
    :server-id 'pwsh-lsp
    :priority 0))
  ;; (repl-driven-development [<f13>] "pwsh -NoProfile -NoLogo" :prompt "PS*>")
  ;; (evil-define-key '(insert normal motion) powershell-mode-map (kbd "C-r") #'pwsh-eval)
  )

(use-package racket-mode
  :hook
  (racket-mode . racket-xp-mode)
  (racket-mode . paredit-mode)
  (racket-mode . aggressive-indent-mode)
  (racket-mode . lsp-deferred))

(use-package go-mode
  :hook
  (go-mode . lsp-deferred)
  :config
  (use-package go-snippets))

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook
  (python-mode . lsp-deferred)
  (python-mode . electric-pair-mode))

(use-package typescript-mode
  :hook
  (typescript-mode . lsp-deferred)
  (typescript-mode . electric-pair-mode))

(use-package yaml-mode
  :bind
  (:map yaml-mode-map
	("C-c C-s" . #'lsp-yaml-select-buffer-schema)
	("C-c C-a" . #'ansible-mode))
  :hook
  (yaml-mode . lsp-deferred)
  :config
  (defun my/yaml-indent (args)
    "docstring"
    (interactive "P")
    (newline)
    (indent-for-tab-command)
    (end-of-line 1)
    (if (= ?-
	   (save-excursion
	     (previous-line 1)
	     (char-after (point))))
	(insert "- ")))
  (evil-define-key '(insert normal motion) yaml-mode-map (kbd "RET") #'my/yaml-indent))

(use-package docker)
(use-package dockerfile-mode
  :hook
  (dockerfile-mode . lsp-deferred))

(use-package jinja2-mode
  :hook
  (jinja2-mode . lsp-deferred)
  :config
  
  (add-to-list 'lsp-language-id-configuration
	       '(jinja2-mode . "jinja"))
  (lsp-register-client (make-lsp-client
			:new-connection (lsp-stdio-connection "jinja-lsp")
			:activation-fn (lsp-activate-on "jinja")
			:server-id 'jinja-lsp)))

(use-package nginx-mode
  :hook
  (nginx-mode . lsp-deferred))

(use-package systemd
  :defer t
  :after lsp-mode
  :hook
  (systemd-mode . lsp-deferred)
  :config
  (add-to-list 'lsp-language-id-configuration
	       '(systemd-mode . "systemd"))
  (lsp-register-client (make-lsp-client
			:new-connection (lsp-stdio-connection "systemd-language-server")
			:activation-fn (lsp-activate-on "systemd")
			:server-id 'systemd-lsp)))

(use-package markdown-mode
  :hook
  (markdown-mode . lsp-deferred))

(use-package ansible)

(use-package terraform-mode
  :hook
  (terraform-mode . lsp-deferred)
  (terraform-mode . electric-pair-mode))

(use-package elixir-mode
  :hook
  (elixer-mode . lsp-deferred)
  (elixir-mode . aggressive-indent-mode)
  (elixir-mode . electric-pair-mode))

(use-package ruby-mode
  :ensure nil
  :hook
  (ruby-mode . lsp-deferred)
  (ruby-mode . aggressive-indent-mode)
  (ruby-mode . electric-pair-mode)
  :config
  (repl-driven-development [<f13>] "irb --inf-ruby-mode --echo-on-assignment" :prompt "irb(main):.*>")
  (evil-define-key '(insert normal motion) ruby-mode-map (kbd "C-r") #'irb-eval))

(switch-to-buffer "*scratch*")
