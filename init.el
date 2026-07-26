;; -*- lexical-binding: t; -*

;; If nix is installed, we're assuming our packages were installed
;; with home-manager
(setq use-package-always-ensure
      (if (file-directory-p "/nix") nil t))

;; Enable melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa" . 10)
        ("gnu"   . 5)))

;; Remember last location in files
(save-place-mode 1)

;; Automatically insert matching braces
(electric-pair-mode 1)

;; Stop emacs from modifying init.el
(load (setq custom-file (concat user-emacs-directory "custom-set-variables.el")))

(load (concat user-emacs-directory "user-interface.el"))
(load (concat user-emacs-directory "modal-editing.el"))
(load (concat user-emacs-directory "ide.el"))
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
  ("M-j" . embark-act))

(use-package embark-consult)
(use-package multiple-cursors)
(use-package visual-regexp)
(use-package projectile)

;; (use-package electric-operator)
;; (prettify-symbols-mode)

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package surround
  :bind
  ("C-c a" . surround-insert)
  ("C-c c" . surround-change)
  ("C-c d" . surround-delete)
  ("C-c k" . surround-kill-outer))
