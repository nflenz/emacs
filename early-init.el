;; Disable the builtin package manager
(setq package-enable-at-startup nil)

;; plists provide better performance in deserialization for lsp-mode
(setenv "LSP_USE_PLISTS" "true")
