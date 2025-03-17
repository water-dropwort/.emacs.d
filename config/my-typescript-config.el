;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルの内容
;; - TypeScriptの開発で使用する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Programming Mode
;; Emacsが29以上のときはtree-sitterを使用できるのでtypescript-ts-modeを使用する。
;; 29未満の場合は使用できないので、typescript-modeを使用する。
(leaf typescript-mode
  :when (version< emacs-version "29")
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'biome-format-on-save-mode)
  )

(leaf typescript-ts-mode
  :ensure nil
  :when (version<= "29" emacs-version)
  :config
  ;; interfaceステートメントでインデントされなかったのでインデントルールを追加
  (defun my/typescript-indent-rules (language)
    (let* ((typescript-rules (cdr (assoc language (typescript-ts-mode--indent-rules language)))))
      (add-to-list 'typescript-rules
                   `((parent-is "interface_body") parent-bol typescript-ts-mode-indent-offset))
      (add-to-list 'typescript-rules
                   `((and (parent-is "interface_body") (node-is "}")) parent-bol 0))
      (setq-local treesit-simple-indent-rules nil)
      (add-to-list 'treesit-simple-indent-rules
                   `(,language ,@typescript-rules))))
  ;; *-ts-modeにhookが存在しないので定義する
  (defvar my/typescript-ts-mode-hook nil)
  (defvar my/tsx-ts-mode-hook nil)
  (defun run-my/typescript-ts-mode-hook ()
    (run-hooks 'my/typescript-ts-mode-hook))
  (defun run-my/tsx-ts-mode-hook ()
    (run-hooks 'my/tsx-ts-mode-hook))
  (advice-add 'typescript-ts-mode :after #'run-my/typescript-ts-mode-hook)
  (advice-add 'tsx-ts-mode        :after #'run-my/tsx-ts-mode-hook)
    ;; .ts,.tsx拡張子のファイルを開いたときにモードを設定する
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . (lambda () (typescript-ts-mode))))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . (lambda () (tsx-ts-mode))))
  )

(leaf tide
  :ensure t
  :config
  ;; Dockerを開発環境とし、Trampでリモートアクセスしたときにtsserverを使用できるようにするための設定。
  (defun my/tide-convert-to-localpath (filepath)
    (if (string-match "^/docker:\\([^:]+\\):" filepath)
        (replace-regexp-in-string "^/docker:\\([^:]+\\):" "" filepath)
      filepath))

  (advice-add 'tide-locate-tsserver-executable
              :filter-return #'my/tide-convert-to-localpath)
  (advice-add 'tide-buffer-file-name
              :filter-return #'my/tide-convert-to-localpath)
  ;; tideのlogファイルを出力するためのオプション
  ;;(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /app/tss.log"))
  )

;; biomeの設定
;;(leaf biomejs-format
;;  :ensure t
;;  :config
;;  ;; biomeをグローバルインストールしていない想定。
;;  ;; biomeの実行ファイルがnode_modulesにあるのでパスを通す。
;;  (defun my/setup-biome-path ()
;;    (let ((root (locate-dominating-file buffer-file-name "package.json")))
;;      (when root
;;        (add-to-list 'exec-path (expand-file-name "node_modules/.bin" (my/tide-convert-to-localpath root)))
;;        )))
;;  ;; ファイルを開いたときにpackage.jsonを探して、biomeの実行ファイルのパスを通すようにする。
;;  (add-hook 'find-file-hook 'my/setup-biome-path)
;;  )

(leaf reformatter
  :ensure t
  :config
  ;; biomeの実行ファイルのパスを通す
  (defun my/setup-biome-path ()
    (let ((root (locate-dominating-file buffer-file-name "package.json")))
      (when root
        (add-to-list 'exec-path (expand-file-name "node_modules/.bin" root)))))
  ;; フォーマッタを設定
  (reformatter-define biome-format
    :program "biome"
    :args `("format" "--stdin-file-path" ,(buffer-file-name))
    :lighter " BiomeFmt")
  )

;; TypeScript環境セットアップ関数
(defun my/setup-typescript-mode ()
  (tide-setup)
  (flymake-mode 0)
  (flycheck-mode 1)
  (my/setup-biome-path)
  (biome-format-on-save-mode)
  )

;; ts,tsxファイルを開いたときにモードが有効になるようにする。
(when (version< emacs-version "29")
  (add-hook 'typescript-mode-hook 'my/setup-typescript-mode)
  )
(when (version<= "29" emacs-version)
  (defun my/setup-typescript-ts-mode ()
    (my/setup-typescript-mode)
    (my/typescript-indent-rules (if (eq major-mode 'typescript-ts-mode) 'typescript 'tsx))
    )
  (add-hook 'my/typescript-ts-mode-hook 'my/setup-typescript-ts-mode)
  (add-hook 'my/tsx-ts-mode-hook 'my/setup-typescript-ts-mode)
  )

(provide 'my-typescript-config)
