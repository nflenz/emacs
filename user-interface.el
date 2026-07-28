;; -*- lexical-binding: t; -*-

;; Disable the startup help screen
(setq inhibit-startup-message t)

;; Unwanted GUI features
(menu-bar-mode -1)
(when (display-graphic-p) (tool-bar-mode -1))
(when (display-graphic-p) (scroll-bar-mode -1))

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

;; Show relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

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
  :custom
  (corfu-on-exact-match nil)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay 0)
  (corfu-count 30)
  (corfu-auto-prefix 0)
  :bind
  ("C-<tab>" . #'completion-at-point)
  :init  
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (keymap-unset corfu-map "RET")

  ;; Make corfu friendly with yas
  (defun my/tab (args)
    "Overload that tab key"
    (interactive "P")
    (if (not (condition-case err
		 (corfu-complete)
               (error nil)))
	(if (not (condition-case err
		     (yas-expand)
		   (error nil)))
            (if (not (condition-case err
			 (progn
			   (yas-next-field)
			   t)
		       (error nil)))
		(indent-for-tab-command)))))

  ;; (evil-define-key '(normal insert) corfu-map (kbd "<tab>") #'my/tab)
  ;; (evil-define-key '(normal insert) global-map (kbd "<tab>") #'my/tab)

  ;; (evil-define-key '(normal) corfu-map (kbd "<tab>") #'my/tab)
  ;; (evil-define-key '(insert) global-map (kbd "<tab>") #'my/tab)
  ;; (evil-define-key '(normal) corfu-map (kbd "M-e") #'corfu-next)
  ;; (evil-define-key '(normal) corfu-map (kbd "M-u") #'corfu-previous)
  ;; (evil-define-key '(insert) global-map (kbd "M-e") #'corfu-next)
  ;; (evil-define-key '(insert) global-map (kbd "M-u") #'corfu-previous)

  ;; ;; Normal movement should just quit corfu
  ;; (evil-define-key '(normal) corfu-map "e" (lambda (args)
  ;; 					     (interactive "P")
  ;; 					     (corfu-quit)
  ;; 					     (next-line args)))
  ;; (evil-define-key '(normal) corfu-map "u" (lambda (args)
  ;; 					     (interactive "P")
  ;; 					     (corfu-quit)
  ;; 					     (previous-line args)))

  
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
	 (consult-completion-in-region beg end table pred)))))

  (with-eval-after-load 'corfu
    ;; Bind the command to a key of your choice (e.g., M-m)
    (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
    ;; Prevent Corfu from automatically closing when invoking this command
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)))

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
  :after cape)

(use-package beacon
  :config
  (beacon-mode))

(use-package eldoc-box)
