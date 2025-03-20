;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルの内容
;; - Emacsのプログラミング環境全体で使用する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 補完
(leaf company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 10)
  :defer-config
  (set-face-attribute 'company-tooltip-selection nil :box t)
  )

;; lspクライアント
(leaf eglot
  :ensure t
  :require t
  :init
  ;; compat(Verticoの依存パッケージ)でrequire-wtih-checkが定義され、
  ;; eglotの初期化処理がうまく動作しない。
  (leaf compat
    :init (fset 'require-with-check nil))
  )

;; 対応するカッコを色分けする
(leaf rainbow-delimiters
  :ensure t
  :require t
  :config
  (set-face-attribute 'rainbow-delimiters-base-error-face nil :foreground "pink"))

;; 変更箇所をハイライトする
(leaf diff-hl
  :ensure t)

;; プログラミング環境セットアップ
(defun my/setup-programming-env ()
  (company-mode)
  (rainbow-delimiters-mode-enable)
  (diff-hl-mode)
  )
(add-hook 'prog-mode-hook 'my/setup-programming-env)

(provide 'my-basic-programming-config)
