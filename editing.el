;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Movement                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
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
      (avy-goto-line))))

(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "C-a") #'my/beginning-of-line)
(global-set-key (kbd "C-e") #'my/end-of-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Copying/Killing                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically delete highlighted text when I start typing
(delete-selection-mode 1)

(defun my/duplicate-line (args)
  "Insert a copy of the current line"
  (interactive "p")
  (let* ((kill-ring kill-ring)
	 (position (point)))    
    (beginning-of-line)
    (kill-line 1)
    (yank)
    (yank)
    (goto-char position)
    (next-line)))

(defun my/kill-whole-line (args)
  "Kill the current line while saving cursor position"
  (interactive "P")
  (let ((position (point)))
    (kill-whole-line args)
    (goto-char position)))

(defun my/kill-line-backwards (args)
  "Kill text from point to indentation"
  (interactive "p")
  (kill-line (+ 1 (- args)))
  (indent-for-tab-command))

(defun my/kill-sexp-backwards (args)
  "kill-sexp in the opposite direction"
  (interactive "p")
  (kill-sexp (- args)))

(global-set-key (kbd "C-c l") #'my/duplicate-line)
(global-set-key (kbd "M-k") #'my/kill-line-backwards)
(global-set-key (kbd "C-c DEL") #'my/kill-sexp-backwards)
(global-set-key (kbd "C-x C-d") #'kill-whole-line)

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

(use-package expand-region
  :bind
  ("M-o" . er/mark-outside-pairs)
  ("M-i" . er/mark-inside-pairs)
  ("M-r" . er/expand-region)
  :custom
  (er/try-expand-list
   '(er/mark-inside-quotes
     er/mark-outside-quotes)))

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
  ("C-c o" . crux-smart-open-line)
  ("C-c O" . crux-smart-open-line-above))

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
