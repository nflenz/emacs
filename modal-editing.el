;; -*- lexical-binding: t; -*-

(use-package ryo-modal
  :commands ryo-modal-mode
  :demand t
  :bind
  ("C-t" . modal/start)
  :config
  (ryo-modal-keys
   ("r" exchange-point-and-mark)
   ("v" yank)
   ("V" consult-yank-pop)
   ("c" kill-ring-save)
   ("." ryo-modal-repeat)
   ("z" undo))

  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                     Entering and exiting ryo-modal-mode                    ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun modal/start ()
    "Change the cursor to a block and start ryo-modal-mode"
    (interactive)
    (send-string-to-terminal "\e[2 q")
    (ryo-modal-mode 1))

  (defun modal/stop ()
    "Change the curor to a vertical bar and stop ryo-modal-mode"
    (interactive)
    (send-string-to-terminal "\e[6 q")
    (ryo-modal-mode -1))

  (defun modal/insert ()
    "Deactivate the region and stop ryo-modal-mode"
    (interactive)
    (deactivate-mark)
    (modal/stop))

  (defun modal/sync-cursor (&optional _)
    "Sync the terminal cursor shape with the current modal state."
    ;; The `unless` check prevents errors if you ever run Emacs in a GUI
    (unless (display-graphic-p) 
      (if ryo-modal-mode
          (send-string-to-terminal "\e[2 q")
	(send-string-to-terminal "\e[6 q"))))

  (add-hook 'ryo-modal-mode-hook #'modal/sync-cursor)
  (add-hook 'window-selection-change-functions #'modal/sync-cursor)
  (add-hook 'window-buffer-change-functions #'modal/sync-cursor)

  (ryo-modal-keys
   ("t" modal/insert))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                 Characters                                 ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun modal/forward-char (args)
    "docstring"
    (interactive "P")
    (deactivate-mark)
    (forward-char args))

  (defun modal/backward-char (args)
    "docstring"
    (interactive "P")
    (deactivate-mark)
    (backward-char args))

  (defun modal/previous-line (args)
    "docstring"
    (interactive "P")
    (deactivate-mark)
    (previous-line args))

  (defun modal/next-line (args)
    "docstring"
    (interactive "P")
    (deactivate-mark)
    (next-line args))

  (defun modal/select-backward-char (args)
    "docstring"
    (interactive "P")
    (unless (use-region-p) (call-interactively #'set-mark-command))
    (backward-char args))

  (defun modal/select-forward-char (args)
    "docstring"
    (interactive "P")
    (unless (use-region-p) (call-interactively #'set-mark-command))
    (forward-char args))

  (defun modal/select-previous-line (args)
    "docstring"
    (interactive "P")
    (unless (use-region-p) (call-interactively #'set-mark-command))
    (previous-line args))

  (defun modal/select-next-line (args)
    "docstring"
    (interactive "P")
    (unless (use-region-p) (call-interactively #'set-mark-command))
    (next-line args))

  (ryo-modal-keys
   ("n" modal/backward-char)
   ("i" modal/forward-char)
   ("u" modal/previous-line)
   ("e" modal/next-line)
   ("N" modal/select-backward-char)
   ("I" modal/select-forward-char)
   ("U" modal/select-previous-line)
   ("E" modal/select-next-line))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                    Words                                   ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun modal/forward-word (args)
    "docstring"
    (interactive "P")
    (deactivate-mark)
    (forward-word args)
    (backward-word args)
    (call-interactively #'set-mark-command)
    (forward-word args))

  (defun modal/backward-word (args)
    "docstring"
    (interactive "P")
    (deactivate-mark)
    (backward-word args)
    (forward-word args)
    (call-interactively #'set-mark-command)
    (backward-word args))

  (defun modal/extend-selection-backward-word (args)
    "docstring"
    (interactive "P")
    (unless (use-region-p)
      (call-interactively #'set-mark-command))
    (backward-word args))

  (defun modal/extend-selection-forward-word (args)
    "docstring"
    (interactive "P")
    (unless (use-region-p)
      (call-interactively #'set-mark-command))
    (forward-word args))

  (ryo-modal-keys
   ("l" modal/backward-word)
   ("y" modal/forward-word)
   ("L" modal/extend-selection-backward-word)
   ("Y" modal/extend-selection-forward-word))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                    Lines                                   ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun modal/end-of-line-or-paragraph (args)
    "Go to end of line, or end of paragraph, or next paragraph in that order of precedence"
    (interactive "P")

    ;; Move to the end of the line if not already there
    (if (not (eq (point) (save-excursion (end-of-line) (point))))
	(progn
	  (deactivate-mark)
	  (call-interactively #'set-mark-command)
	  (end-of-line))

      ;; Move to the end of the paragraph if not already there
      (if (not (eq (point) (save-excursion (forward-paragraph) (backward-char 1) (point))))
	  (progn
	    (unless (use-region-p) (call-interactively #'set-mark-command))
	    (forward-paragraph)
	    (backward-char 1))

	;; Select the next paragraph
	(progn
	  (deactivate-mark)
	  (forward-char 1)
	  (forward-paragraph)
	  (backward-paragraph)
	  (forward-char 1)
	  (back-to-indentation)
	  (call-interactively #'set-mark-command)
	  (forward-paragraph)
	  (backward-char 1)))))

  (defun modal/beginning-of-line-or-paragraph (args)
    "Go to beginning of line, or beginning of paragraph, or previous paragraph in that order of precedence"
    (interactive "P")

    ;; Move to the beginning of the line if not already there
    (if (not (eq (point) (save-excursion (back-to-indentation) (point))))
	(progn
	  (deactivate-mark)
	  (call-interactively #'set-mark-command)
	  (back-to-indentation))

      ;; Move to the beginning of the paragraph if not already there
      (if (not (eq (point) (save-excursion (backward-paragraph) (end-of-line) (forward-char 1) (back-to-indentation) (point))))
	  (progn
	    (unless (use-region-p) (call-interactively #'set-mark-command))
	    (backward-paragraph)
	    (forward-char)
	    (back-to-indentation))

	;; Highlight the previous paragraph
	(progn
	  (deactivate-mark)
	  (backward-paragraph 2)
	  (forward-paragraph)
	  (previous-line)
	  (end-of-line)
	  (call-interactively #'set-mark-command)
	  (backward-paragraph)
	  (next-line)
	  (back-to-indentation)))))

  (ryo-modal-keys
   ("h" modal/beginning-of-line-or-paragraph)
   ("o" modal/end-of-line-or-paragraph))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                  Searching                                 ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package avy
    :custom
    (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

  (defun modal/jump (args)
    "Start a region from current point to destination selected with avy-goto-char-timer"
    (interactive "P")
    (deactivate-mark)
    (call-interactively #'set-mark-command)
    (avy-goto-char-timer))

  (ryo-modal-keys
   ("j" modal/jump))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                  Actions                                   ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun modal/change (args)
    "kill-region if active then disable ryo-modal-mode"
    (interactive "P")
    (if (use-region-p)
	(call-interactively #'kill-region)
      (back-to-indentation)
      (kill-line args))
    (modal/stop))

  (defun modal/delete (args)
    "If region is active then kill-region. If region isn't active then kill-whole-line"
    (interactive "P")
    (if (use-region-p)
	(progn
	  (call-interactively #'kill-region)
	  (xah-shrink-whitespaces))
      (kill-whole-line args)))

  ;; (defun modal/delete-paragraph (args)
  ;;   "Delete the current paragraph"
  ;;   (interactive "P")
  ;;   (er/mark-text-paragraph)
  ;;   (call-interactively #'kill-region)
  ;;   (xah-shrink-whitespaces))

  (defun modal/comment (args)
    "Comment region or current line"
    (interactive "P")
    (if (use-region-p)
	(comment-dwim 1)
      (comment-line args)))

  ;; (defun modal/comment-paragraph (args)
  ;;   "Comment the current paragraph"
  ;;   (interactive "P")
  ;;   (er/mark-text-paragraph)
  ;;   (comment-dwim 1))

  (defun modal/open-line-below (args)
    "Insert a new line below the current one"
    (interactive "P")
    (end-of-line)
    (newline)
    (indent-for-tab-command)
    (modal/stop))
  
  (defun modal/open-line-above (args)
    "Insert a new line below the current one"
    (interactive "P")
    (previous-line)
    (end-of-line)
    (newline)
    (indent-for-tab-command)
    (modal/stop))

  (ryo-modal-keys
   ("s" modal/change)
   ("x" modal/delete)
   ("f" modal/comment)))

;; Just needed for some functions that I like
(use-package xah-fly-keys
  :demand t
  :ensure t
  :custom
  (xah-fly-use-control-key nil)
  :bind
  ("C-x k" . xah-close-current-buffer)  
  ("C-x C-b" . xah-next-user-buffer)
  :config
  (xah-fly-keys-set-layout "colemak")
  ;; (define-key ryo-modal-mode-map (kbd "SPC") xah-fly-leader-key-map)
  (ryo-modal-keys
   ("w" xah-shrink-whitespaces)
   ("c" xah-copy-line-or-region)))

(use-package expand-region
  :ensure t
  :config
  (ryo-modal-keys
   ("a" er/expand-region)
   ("m" er/mark-defun)
   ("," er/mark-inside-pairs)
   ("." er/mark-outside-pairs)))
