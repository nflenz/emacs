;; Basic Settings
(org-babel-load-file "~/.emacs.d/settings.d/packages.org")
(org-babel-load-file "~/.emacs.d/settings.d/global-settings.org")

;; Tools
(org-babel-load-file "~/.emacs.d/settings.d/version-control.org")

;; Languages/File Formats
(org-babel-load-file "~/.emacs.d/settings.d/org-mode.org")
(org-babel-load-file "~/.emacs.d/settings.d/docker.org")
(org-babel-load-file "~/.emacs.d/settings.d/elisp.org")
(org-babel-load-file "~/.emacs.d/settings.d/shell.org")
(org-babel-load-file "~/.emacs.d/settings.d/kubernetes.org")

;; Start in the scratch buffer
(switch-to-buffer "*scratch*")
