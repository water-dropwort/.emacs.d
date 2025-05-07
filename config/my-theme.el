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

(provide 'my-theme)
