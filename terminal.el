;; -*- lexical-binding: t; -*

(use-package ghostel
  :bind
  (("C-z" . #'ghostel)
   ("C-c z" . #'my/switch-to-ghostel-buffer)
   (:map ghostel-semi-char-mode-map
	 ("C-z" . #'ghostel))
   (:map ghostel-line-mode-map
	 ("C-z" . #'ghostel)))
  
  :init
  ;; Set ghostel as my default buffer
  ;; Also launch ghostel if it isn't running
  (setq initial-buffer-choice
	(lambda ()
          ;; Start ghostel if the buffer doesn't already exist
          (unless (get-buffer "*ghostel*")
            (ghostel))
          ;; Return the ghostel buffer to display it
          (get-buffer "*ghostel*")))

  ;; Put the current directory in the buffer name
  (setq ghostel-buffer-name-function
	(lambda (_title)
          (format "*ghostel: %s*" (abbreviate-file-name default-directory)))))

(defun my/switch-to-ghostel-buffer ()
  "Run `project-switch-to-buffer` and pre-fill the minibuffer with `*ghostel `."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert "*ghostel "))
    (call-interactively #'projectile-switch-to-buffer)))
