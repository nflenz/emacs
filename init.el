;; -*- lexical-binding: t; -*

;; Stop emacs from modifying init.el
(load (setq custom-file (concat user-emacs-directory "custom-set-variables.el")))

(load (concat user-emacs-directory "elpaca.el"))
(load (concat user-emacs-directory "user-interface.el"))
(load (concat user-emacs-directory "modal-editing.el"))
(load (concat user-emacs-directory "ide.el"))
(load (concat user-emacs-directory "temp-fixes.el"))

(use-package eat)

(use-package gptel)

(use-package dired
  :ensure nil)

(use-package vundo
  :bind
  ("C-x u" . vundo))

(use-package embark
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))
(use-package embark-consult)

(use-package beacon
  :config
  (beacon-mode))

(use-package multiple-cursors)

(use-package visual-regexp-steroids)

(use-package dap-mode)

(use-package projectile)

(use-package just-ts-mode)

;; (use-package electric-operator)
;; (prettify-symbols-mode)

(use-package ctrlf
  :config
  (ctrlf-mode +1))
