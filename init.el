(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/lib" user-emacs-directory))

(require 'my-init-leaf)
(require 'my-basic-config)
(require 'my-basic-programming-config)
(require 'my-typescript-config)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))




;; .plファイルを開いたときはProlog-modeを適用する。
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third party packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  ;; Faceを一部上書き
  (set-face-attribute 'default nil :height 100 :background "#101010")
  (set-face-attribute 'region nil :background "#77d9a8" :foreground "black")
  (set-face-attribute 'font-lock-string-face nil :foreground "#f6aa00")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#77d9a8")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#77d9a8"))

(leaf vertico
  :ensure t
  :init
  (vertico-mode t)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 5))








(leaf yaml-mode
  :ensure t)

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

