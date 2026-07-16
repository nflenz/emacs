;; -*- lexical-binding: t; -*-

;; Needed for minad's packages and isn't being installed by elpaca for some reason
(use-package compat
  :after elpaca
  :ensure t
  :demand t)

(unless (fboundp 'set-local)
  (defun set-local (variable value)
    "Make VARIABLE buffer-local and set it to VALUE."
    (set (make-local-variable variable) value)))
