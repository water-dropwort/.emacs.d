;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルの内容
;; - Emacs全体で使用する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq delete-auto-save-files t)

;; デフォルトでセットされているキーバインドを解除する。
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-\\"))

;; Alt+上下キーで行を移動できるようにする。
(require 'my-move-line)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<up>") 'move-line-up)

;; バッファ移動キーバインド
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)
;; バッファ入れ替えキーバインド
(global-set-key (kbd "C-c S-<left>") 'windmove-swap-states-left)
(global-set-key (kbd "C-c S-<down>") 'windmove-swap-states-down)
(global-set-key (kbd "C-c S-<up>") 'windmove-swap-states-up)
(global-set-key (kbd "C-c S-<right>") 'windmove-swap-states-right)
;; 文字削除系バインド
(global-set-key (kbd "C-S-d") 'delete-backward-char)
;; 一行ずつスクロールされるようにする
(setq scroll-conservatively most-positive-fixnum)
;; UTF-8を優先する
(prefer-coding-system 'utf-8)
;; デフォルトはUTF-8
(set-default-coding-systems 'utf-8)
;; クリップボードへのコピー/からの貼り付けのエンコード/デコードをUTF-8で。
(set-selection-coding-system 'utf-8)
;; Alt+.で定義に飛ぶとき、新しいウィンドウで開く
(define-key esc-map "." 'xref-find-definitions-other-window)
;; 長い行はスクロールしてみるのではなく、折り返す。
(setq-default truncate-lines nil)
;; インデントにタブを使用しない
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; 対になる括弧をハイライトする
(show-paren-mode 1)
(setq show-paren-style 'parenthisis)
;; カーソルがあたっている行をハイライトする
(global-hl-line-mode t)
;; ツールバー非表示
(tool-bar-mode -1)
;; 行番号を表示する
(global-display-line-numbers-mode 1)
;; 括弧を補完する
(electric-pair-mode 1)

;; whitespace-modeの設定
(leaf whitespace
  :require t
  :config
  (custom-set-faces
  '(whitespace-space    ((t (:foreground "lightgray" :background "orangered"))))
  '(whitespace-trailing ((t (:foreground "lightgray" :background "orangered"))))
  '(whitespace-newline  ((t (:foreground "lightgray" :background "gray20"))))
  '(whitespace-tab      ((t (:foreground "lightgray" :background "gray20")))))
  (setq whitespace-style
        '(face spaces space-mark trailing tabs tab-mark newline newline-mark))
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\□])
          (tab-mark ?\t [?\xBB ?\t])
          (newline-mark ?\n [?↵ ?\n] [?$ ?\n])))
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; タブバーの設定
(leaf tab-bar
  :config
  (tab-bar-mode t)
  (set-face-attribute 'tab-bar-tab nil :background "orange" :foreground "black"))

;; dirモードの設定
(leaf dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(file-size subtree-state))
  (define-key dirvish-mode-map (kbd "TAB")       'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "<backtab>") 'dirvish-subtree-remove))

;; Evilモード追加
(leaf evil
  :ensure t
  :config
  (evil-mode))

;; Vertico
(leaf vertico
  :ensure t
  :init
  (vertico-mode t)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 5))

(provide 'my-basic-config)
