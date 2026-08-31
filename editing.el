;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-.") #'kmacro-start-macro)
(global-set-key (kbd "C-,") #'kmacro-end-macro)
(global-set-key (kbd "M-'") #'repeat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Movement                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :custom
  (avy-keys '(?t ?n ?s ?e ?r ?i ?a ?o))
  :bind
  ("M-a" . avy-goto-char-timer))

(defun my/end-of-line (args)
  "Run avy-goto-end-of-line if already at end of line, otherwise run end-of-line"
  (interactive "p")
  (let ((start (point)))
    (end-of-line)
    (if (= start (point))
	(avy-goto-end-of-line))))

(defun my/beginning-of-line (args)
  "Run avy-goto-line if already at indentation, otherwise run back-to-indentation"
  (interactive "p")
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point))
      (avy-goto-line)
      (back-to-indentation))))

(global-set-key (kbd "C-a") #'my/beginning-of-line)
(global-set-key (kbd "C-e") #'my/end-of-line)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Copying/Killing                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically delete highlighted text when I start typing
(delete-selection-mode 1)

(defun my/kill-whole-line (args)
  "Kill the current line while saving cursor position"
  (interactive "P")
  (let ((position (point)))
    (kill-whole-line args)
    (goto-char position)))

(defun my/kill-sexp-backwards (args)
  "kill-sexp in the opposite direction"
  (interactive "p")
  (kill-sexp (- args)))

(global-set-key (kbd "C-c l") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "M-k") #'my/kill-whole-line)
(global-set-key (kbd "C-c DEL") #'my/kill-sexp-backwards)
(global-set-key (kbd "C-M-z") #'zap-up-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Joining                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/join-line-backward (args)
  "Join the current line with the previous line while saving cursor
position"
  (interactive "P")
  (let ((position (point)))
    (join-line args)
    (goto-char position)))

(defun my/join-line-forward (args)
  "Join the current line with the next line while saving cursor
position"
  (interactive "P")
  (let ((position (point)))
    (end-of-line)
    (delete-char 1)
    (insert " ")
    (goto-char position)))

(global-set-key (kbd "C-j") #'my/join-line-backward)
(global-set-key (kbd "M-j") #'my/join-line-forward)
(define-key lisp-interaction-mode-map (kbd "C-j") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Highlighting                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region)

(defun my/mark-inner-quotes-or-pairs ()
  "Try `er/mark-inside-quotes`, and if it fails to change the region, run `er/mark-inside-pairs`."
  (interactive)
  (let ((p (point))
        (m (mark t)))
    (ignore-errors (er/mark-inside-quotes))
    (when (and (eq p (point))
	       (eq m (mark t)))
      (ignore-errors (er/mark-inside-pairs)))))

(defun my/mark-outside-quotes-or-pairs ()
  "Try `er/mark-outer-quotes`, and if it fails to change the region, run `er/mark-outer-pairs`."
  (interactive)
  (let ((p (point))
        (m (mark t)))
    (ignore-errors (er/mark-outside-quotes))
    (when (and (eq p (point))
	       (eq m (mark t)))
      (ignore-errors (er/mark-outside-pairs)))))

(global-set-key (kbd "M-o") #'my/mark-outside-quotes-or-pairs)
(global-set-key (kbd "M-i") #'my/mark-inner-quotes-or-pairs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Pairs                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package surround)

(defun my/surround-dwim (args)
  "Surround region with pressed key if transient mark is set. Otherwise, just insert the key"
  (interactive "p")
  (if (use-region-p)
      (surround-insert (this-command-keys))
    (self-insert-command args (string-to-char (this-command-keys)))))

(global-set-key (kbd "'") #'my/surround-dwim)
(global-set-key (kbd "\"") #'my/surround-dwim)
(global-set-key (kbd "(") #'my/surround-dwim)
(global-set-key (kbd "{") #'my/surround-dwim)
(global-set-key (kbd "[") #'my/surround-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Cases                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/upcase-word-or-region (args)
  "If region is highlighted then run upcase-region, else run upcase-word"
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'upcase-region)
    (upcase-word args)))

(defun my/downcase-word-or-region (args)
  "If region is highlighted then run downcase-region, else run downcase-word"
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'downcase-region)
    (downcase-word args)))

(global-set-key (kbd "C-x C-u") #'my/upcase-word-or-region)
(global-set-key (kbd "C-x C-l") #'my/downcase-word-or-region)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Opening lines                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package crux
  :bind
  ("C-o" . crux-smart-open-line)
  ("C-M-o" . crux-smart-open-line-above))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Pasting                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-y") #'consult-yank-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Whitespace                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/fix-whitespace-or-kill-region (args)
  "Run kill-region if transient mark, else fix whitespace"
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (delete-blank-lines)
    (just-one-space)))

(global-set-key (kbd "C-w") #'my/fix-whitespace-or-kill-region)
