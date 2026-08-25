;; -*- lexical-binding: t; -*-

;; Disable the startup help screen
(setq inhibit-startup-message t)

;; Unwanted GUI features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable those annoying byte compile warnings
(setq byte-compile-warnings nil)

;; Disable backups, autosaves and locks
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Enable the mouse in the terminal
(unless (display-graphic-p) (xterm-mouse-mode 1))

;; Show available keybindings
(which-key-mode 1)

;; The system bell is completely useless and annoying. No thank you.
(setq ring-bell-function 'ignore)

;; This theme seems to look the best on the terminal
(use-package ef-themes
  :init
  (load-theme 'ef-bio))

;; Transparency
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Font size
(set-face-attribute 'default nil :height 140)

;; Better help commands
(use-package helpful
  :bind
  (("C-h f" . 'helpful-callable)
   ("C-h v" . 'helpful-variable)
   ("C-h k" . 'helpful-key)
   ("C-h x" . 'helpful-command)))

;; Minibuffer completions
(use-package vertico
  :init
  (vertico-mode 1))

;; Functions for vertico
(use-package consult
  :bind
  ("C-c s" . consult-line)
  :demand t)

;; Extra information for completion candidates
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Search for candidates without typing strings in order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :init  
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)

  :custom
  (corfu-on-exact-match nil)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay 0)
  (corfu-count 10)
  (corfu-auto-prefix 0)

  :bind
  (:map corfu-map
	("<tab>" . #'corfu-complete)
	("M-N" . #'corfu-next)
	("M-P" . #'corfu-previous)
	("C-n" . #'my/corfu-quit-and-execute)
	("C-p" . #'my/corfu-quit-and-execute)
	("RET" . #'my/corfu-quit-and-execute)
	("M-m" . #'my/corfu-move-to-minibuffer))

  :config  
  (defun my/corfu-quit-and-execute (args)
    "Stop corfu and execute the pressed chord"
    (interactive "p")
    (corfu-quit)
    (execute-kbd-macro (this-command-keys)))

  (defun my/corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
	 (consult-completion-in-region beg end table pred)))))
  
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

;; Display completion candidates in order
(use-package prescient
  :demand t)

(use-package vertico-prescient
  :demand t
  :config
  (vertico-prescient-mode 1))

(use-package corfu-prescient
  :demand t
  :config
  (corfu-prescient-mode 1))

;; Extra completion functions
(use-package cape
  :defer nil
  :bind
  ("M-/" . cape-dabbrev))

;; Show snippets in corfu
(use-package yasnippet-capf
  :demand t
  :bind
  ("C-c c y" . #'yasnippet-capf)
  :after cape)

(use-package beacon
  :config
  (beacon-mode))

(use-package eldoc-box)
