(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/lib" user-emacs-directory))

(require 'my-theme)
(require 'my-init-leaf)
(require 'my-basic-config)
(require 'my-basic-programming-config)
(require 'my-typescript-config)
(require 'my-prog-prolog)
(require 'my-prog-yaml)

(leaf treesit-auto
  :when (version<= "29" emacs-version)
  :ensure t
  :require t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(leaf docker-tramp
  :when (version< emacs-version "29")
  :ensure t
  )

(provide 'init)

