(defun my/move-character-right (args)
  "Move the character at point to the right"
  (interactive "*p")
  (forward-char 1)
  (transpose-chars args)
  (backward-char 1))

(defun my/move-character-left (args)
  "Move the character at point to the left"
  (interactive "*p")
  (my/move-character-right (- args)))

(defmacro my/save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
	 (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun my/move-line-up ()
  "Swap the line at point with the line above it"
  (interactive)
  (my/save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun my/move-line-down ()
  "Swap the line at point with the one below it"
  (interactive)
  (my/save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

(defun my/move-paragraph-down (args)
  "Swap the current paragraph with the one above it while preserving the position in the paragraph"
  (interactive "*p")
  (let* ((initial-point (point))
	 (point-in-paragraph nil))
    (backward-paragraph 1)
    (setq point-in-paragraph (- initial-point (point)))
    (forward-char point-in-paragraph)
    (transpose-paragraphs args)
    (backward-paragraph 1)
    (forward-char point-in-paragraph)))

(defun my/move-paragraph-up (args)
  "Swap the current paragraph with the one below it while preserving the position in the paragraph"
  (interactive "*p")
  (my/move-paragraph-down (- args)))

(defun my/word-start ()
  "Return the point for the beginning of the current word"
  (min
   (save-excursion
     (backward-word)
     (forward-word)
     (backward-word)
     (point))
   (save-excursion
     (forward-word)
     (backward-word)
     (point))))

(defun my/word-end ()
  "Return the point for the end of the current word"
  (min
   (save-excursion
     (backward-word)
     (forward-word)
     (point))
   (save-excursion
     (forward-word)
     (point))))

(defun my/move-word-right (args)
  "Swap the current word with the one after it while preserving the point's position in the word"
  (interactive "*p")

  (let ((point-in-word nil))
    (setq point-in-word
	  (- (point)
	     (my/word-start)))

    (transpose-words args)
    (backward-word)
    (forward-char point-in-word)))

(defun my/move-word-left (args)
  "Swap the current word with the one before it while preserving the point's position in the word"
  (interactive "*p")
  (my/move-word-right (- args)))

(defun my/move-sentence-right (args)
  "Swap the current sentence with the one after it while preserving the point's position in the word"
  (interactive "*p")
  
  (let ((point-in-sentence nil))
    (setq point-in-sentence
	  (- (point)
	     (min
	      (save-excursion
		(backward-sentence)
		(forward-sentence)
		(point))
	      (save-excursion
		(forward-sentence)
		(point)))))

    (transpose-words args)
    (backward-word)
    (forward-char point-in-word))
  )
