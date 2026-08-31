;; -*- lexical-binding: t; -*

;; If nix is installed, we're assuming our packages were installed
;; with home-manager
(setq use-package-always-ensure t)
(load (concat user-emacs-directory "elpaca.el"))

(if (file-exists-p "~/.proxy.el")
    (load "~/proxy.el"))

;; ;; Enable melpa
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (setq package-archive-priorities
;;       '(("melpa" . 10)
;;         ("gnu"   . 5)))

;; Remember last location in files
(save-place-mode 1)

;; Automatically insert matching braces
(electric-pair-mode 1)

;; Stop emacs from modifying init.el
(load (setq custom-file (concat user-emacs-directory "custom-set-variables.el")))

(load (concat user-emacs-directory "user-interface.el"))
(load (concat user-emacs-directory "editing.el"))
;;(load (concat user-emacs-directory "modal-editing.el"))
(load (concat user-emacs-directory "ide.el"))
(load (concat user-emacs-directory "lsp-booster.el"))
(load (concat user-emacs-directory "terminal.el"))
(load (concat user-emacs-directory "temp-fixes.el"))

(use-package eat)
(use-package gptel)

(use-package vundo
  :bind
  ("C-x u" . vundo))

(use-package embark
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  :bind
  ("M-e" . embark-act))

(use-package embark-consult)
(use-package multiple-cursors)
(use-package visual-regexp)

(use-package projectile
  :custom
  (projectile-switch-project-action #'projectile-vc)
  :bind
  ("C-x p" . 'projectile-command-map))

;; (use-package electric-operator)
;; (prettify-symbols-mode)

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package fish-completion
  :hook
  (ghostel-line-mode . #'fish-completion-mode))
