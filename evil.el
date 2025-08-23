;; evil mode and related functions

(use-package evil
  :defer nil

  :custom
  (evil-want-minibuffer t)
  (evil-highlight-closing-paren-at-point-states '(emacs insert replace))
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)

  :config
  (evil-mode 1)

  (defun my/find-file (args)
    "Run project-find-file without project defined if run from minibuffer"
    (interactive "P")
    (if (not (minibufferp))
	(if (project-current) (call-interactively #'projectile-find-file)
	  (call-interactively #'find-file))
      (embark--quit-and-run
       #'(lambda nil
	   (let* ((default-directory "/"))
	     (call-interactively #'projectile-find-file))))))

  (defun my/find-dir (args)
    "Run project-find-dir without project defined if run from minibuffer"
    (interactive "P")
    (if (not (minibufferp))
	(if (project-current) (call-interactively #'projectile-find-dir)
	  (call-interactively #'find-dired))
      (embark--quit-and-run
       #'(lambda nil
	   (let* ((default-directory "/"))
	     (call-interactively #'projectile-find-dir))))))

  (defun my/kill-current-buffer (args)
    "docstring"
    (interactive "P")
    (if (eq (current-buffer) (get-buffer "*scratch*"))
	(message "Cowardly refusing to delete the scratch buffer")
      (kill-current-buffer)))

  (evil-define-key '(normal motion insert) global-map (kbd "M-r") #'my/kill-current-buffer)
  (evil-define-key '(normal motion insert) global-map (kbd "M-t") #'my/find-file)
  (evil-define-key '(normal motion insert) global-map (kbd "M-d") #'my/find-dir)

  (defun my/evil-space-then-insert (args)
    "Insert a space and then enter insert mode"
    (interactive "P")
    (insert " ")
    (evil-insert 0))

  (defun my/evil-join-left (args)
    "Join current line with previous line without losing position"
    (interactive "P")
    (save-excursion
      (previous-line 1)
      (call-interactively #'evil-join)))

  (defun my/evil-join-right (args)
    "Run evil-join without losing position"
    (interactive "P")
    (save-excursion (call-interactively #'evil-join)))

  (defun my/evil-eol-comment (args)
    "Add a comment at eol"
    (interactive "P")
    (call-interactively #'comment-dwim)
    (insert " ")
    (evil-insert 0))

  (defun my/pop-mark-ring ()
    "Jump to previous mark"
    (interactive)
    (set-mark-command t))

  (defun my/yank-below (args)
    "Yank text below current line"
    (interactive "P")
    (end-of-line 1)
    (newline 1)
    (yank 1))

  (defun my/evil-ex-search-next (args)
    "evil-ex-search-next with a consistent direction"
    (interactive "P")
    (let* ((evil-ex-search-direction 'forward))
      (call-interactively #'evil-ex-search-next)))

  (defun my/evil-ex-search-previous (args)
    "evil-ex-search-previous with a consistent direction"
    (interactive "P")
    (let* ((evil-ex-search-direction 'forward))
      (call-interactively #'evil-ex-search-previous)))

  (evil-define-text-object my/evil-WORD-dwim (count &optional beg end type)
    "Define a text object for WORDs"
    (let* ((regexp (rx (any " {}()")))
	   (beg (save-excursion (+ (search-backward-regexp regexp) 1)))
	   (end (save-excursion (search-forward-regexp regexp)))
	   (chars (list (char-to-string (char-before beg))
			(char-to-string (char-before end)))))
      (if (eq this-command 'evil-change)
	  (list beg end)
	;; fix below to account for evil-delete
	(list beg end))))

  ;; It is my opinion that evil-change should always use the 'inner'
  ;; function, and evil-delete should always use the 'a' function
  (dolist (type '("word" "paragraph" "paren" "curly" "bracket" "angle" "single-quote" "double-quote" "sentence" "back-quote"))
    (eval
     `(defun ,(intern (concat "my/evil-" type "-dwim")) (args)
	(interactive "P")
	(call-interactively
	 (if (eq this-command #'evil-change)
             (function ,(intern (concat "evil-inner-" type)))
           (function ,(intern (concat "evil-a-" type))))))))

  (evil-define-operator my/evil-comment (beg end type)
    "Comment or uncomment region using comment-dwim"
    :move-point nil
    (interactive "<R>")
    (if (and beg end)
	(progn
	  (comment-or-uncomment-region beg end)
	  (when (eq type 'line)
	    (evil-first-non-blank)))
      (comment-dwim nil))
    (when (and (eq type 'line)
	       (not (save-excursion
		      (goto-char end)
		      (eolp))))))

  ;; (evil-define-operator my/evil-copy-to-register (beg end type)
  ;;   "Copy region to a register"
  ;;   :move-point nil
  ;;   (interactive "<R>")
  ;;   (if (and beg end)
  ;; 	(progn
  ;; 	  ;; (comment-or-uncomment-region beg end)
  ;; 	  (when (eq type 'line)
  ;; 	    (evil-first-non-blank)))
  ;;     (comment-dwim nil))
  ;;   (when (and (eq type 'line)
  ;; 	       (not (save-excursion
  ;; 		      (goto-char end)
  ;; 		      (eolp))))))


  ;; Some operator ideas
  ;; - Case toggle operators
  ;; - Surround operators
  ;; - Sort operator
  ;; - Refactoring operators
  ;; - Split and join operators
  ;; - Macro operator

  ;; More bindings to map:
  ;; - Macros
  ;; - Mark ring
  ;; - Position Registers [ point-to-register, jump-to-register ]
  ;; - Text Registers [ copy-to-register, insert-register ]
  ;; - Windows [ window-configuration-to-register, other-window, split-window-below, split-window-right, delete-other-windows ]
  ;; -

  (evil-define-key '(normal insert motion) global-map (kbd "<tab>") #'indent-for-tab-command)

  (evil-define-key '(normal motion insert) global-map (kbd "M-a") #'save-buffer)
  (evil-define-key '(normal motion insert) global-map (kbd "M-s") #'switch-to-buffer) 

  (evil-define-key '(normal motion insert) global-map (kbd "M-x") #'split-window-right)
  (evil-define-key '(normal motion insert) global-map (kbd "M-c") #'split-window-below)
  (evil-define-key '(normal motion insert) global-map (kbd "M-v") #'delete-other-windows)
  (evil-define-key '(normal motion insert) global-map (kbd "M-b") #'delete-window)

  (evil-define-key '(normal operator) global-map "a" #'execute-extended-command)
  (evil-define-key '(normal visual) global-map "t" #'evil-insert)
  (evil-define-key '(normal visual) global-map "." #'evil-repeat)
  (evil-define-key '(normal visual) global-map "/" #'evil-undo)

  (evil-define-key '(normal) global-map "d" #'yank)
  (evil-define-key '(normal) global-map "D" #'my/yank-below)
  ;; (evil-define-key '(normal) global-map "v" #'yank)
  ;; (evil-define-key '(normal) global-map (kbd "C-d") #'delete-char)
  (evil-define-key '(normal) global-map "A" #'evil-ex)
  (evil-define-key '(normal) global-map "F" #'my/evil-eol-comment)
  (evil-define-key '(normal) global-map "U" #'evil-open-above)
  (evil-define-key '(normal) global-map "E" #'evil-open-below)
  (evil-define-key '(normal) global-map "-" #'negative-argument)
  (evil-define-key '(normal) global-map "'" #'delete-char)
  (evil-define-key '(normal) global-map (kbd "DEL")#'backward-delete-char)
  (evil-define-key '(normal) global-map (kbd "SPC") #'my/evil-space-then-insert) 
  (evil-define-key '(normal) global-map (kbd "RET")#'newline)
  (evil-define-key '(normal) global-map (kbd "<S-Return>") #'open-line)

  ;; Search
  (evil-define-key '(normal motion insert operator) global-map (kbd "M-n") #'evil-ex-search-backward)
  (evil-define-key '(normal motion insert operator) global-map (kbd "M-i") #'evil-ex-search-forward)
  (evil-define-key '(normal motion insert operator) global-map (kbd "M-N") #'my/evil-ex-search-previous)
  (evil-define-key '(normal motion insert operator) global-map (kbd "M-I") #'my/evil-ex-search-next)

  ;; Buffer traversal
  (evil-define-key '(normal visual insert) global-map (kbd "M-E") #'scroll-up-command)
  (evil-define-key '(normal visual insert) global-map (kbd "M-U") #'scroll-down-command)
  (evil-define-key '(normal visual insert) global-map (kbd "M-h") #'beginning-of-buffer)
  (evil-define-key '(normal visual insert) global-map (kbd "M-o") #'end-of-buffer)

  ;; Joins
  (evil-define-key '(normal visual insert) global-map (kbd "M-l") #'my/evil-join-left)
  (evil-define-key '(normal visual insert) global-map (kbd "M-y") #'my/evil-join-right)

  ;; Directions
  (evil-define-key '(normal visual) global-map "u" #'previous-line)
  (evil-define-key '(normal visual) global-map "e" #'next-line)
  (evil-define-key '(normal visual) global-map "n" #'backward-char)
  (evil-define-key '(normal visual) global-map "i" #'forward-char)

  ;; Words
  (evil-define-key '(normal visual operator) global-map "l" #'backward-word)
  (evil-define-key '(normal visual operator) global-map "y" #'forward-word)

  ;; Sentences
  (evil-define-key '(normal visual operator) global-map "L" #'backward-sentence)
  (evil-define-key '(normal visual operator) global-map "Y" #'forward-sentence)

  ;; Lines
  (evil-define-key '(normal visual operator) global-map "h" #'back-to-indentation)
  (evil-define-key '(normal visual operator) global-map "o" #'end-of-line)

  ;; Sexps
  (evil-define-key '(normal visual operator) global-map "N" #'backward-sexp)
  (evil-define-key '(normal visual operator) global-map "I" #'forward-sexp)

  ;; Paragraphs
  (evil-define-key '(normal visual operator) global-map "H" #'backward-paragraph)
  (evil-define-key '(normal visual operator) global-map "O" #'forward-paragraph)

  ;; Transpositions
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-n") #'my/move-character-left)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-i") #'my/move-character-right)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-u") #'my/move-line-up)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-e") #'my/move-line-down)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-h") #'my/move-paragraph-up)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-o") #'my/move-paragraph-down)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-l") #'my/move-word-left)
  (evil-define-key '(normal insert motion) global-map (kbd "C-S-y") #'my/move-word-right)

  ;; Operators
  (evil-define-key 'normal global-map "w" #'evil-yank)
  (evil-define-key 'normal global-map "f" #'my/evil-comment)
  (evil-define-key 'normal global-map "r" #'evil-delete)
  (evil-define-key 'normal global-map "s" #'evil-change)
  (evil-define-key 'normal global-map "P" #'evil-upcase)
  (evil-define-key 'normal global-map "T" #'evil-downcase)

  ;; Operator selections
  (evil-define-key 'operator global-map "u" #'my/evil-word-dwim)
  (evil-define-key 'operator global-map "e" #'my/evil-WORD-dwim)
  (evil-define-key 'operator global-map "z" #'my/evil-angle-dwim)
  (evil-define-key 'operator global-map "x" #'my/evil-bracket-dwim)
  (evil-define-key 'operator global-map "c" #'my/evil-curly-dwim)
  (evil-define-key 'operator global-map "v" #'my/evil-paren-dwim)
  (evil-define-key 'operator global-map "b" nil)
  (evil-define-key 'operator global-map "k" #'my/evil-sentence-dwim)
  (evil-define-key 'operator global-map "m" #'my/evil-paragraph-dwim)
  (evil-define-key 'operator global-map "," #'my/evil-double-quote-dwim)
  (evil-define-key 'operator global-map "." #'my/evil-single-quote-dwim)
  (evil-define-key 'operator global-map "/" #'my/evil-back-quote-dwim))
