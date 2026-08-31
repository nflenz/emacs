;; -*- lexical-binding: t; -*

(use-package ghostel
  :bind
  (("C-z" . #'projectile-run-ghostel)
   (:map ghostel-semi-char-mode-map
	 ("C-z" . #'my/switch-to-ghostel-buffer))
   (:map ghostel-line-mode-map
	 ("C-z" . #'my/switch-to-ghostel-buffer)))
  
  :init
  ;; Set ghostel as my default buffer
  ;; Also launch ghostel if it isn't running
  (setq initial-buffer-choice
	(lambda ()
          ;; Start ghostel if the buffer doesn't already exist
          (unless (get-buffer "*ghostel*")
            (ghostel))
          ;; Return the ghostel buffer to display it
          (get-buffer "*ghostel*"))))

(defun my/switch-to-ghostel-buffer ()
  "Run `consult-buffer` and pre-fill the minibuffer with `*ghostel `."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert "*ghostel "))
    (call-interactively #'consult-buffer)))
