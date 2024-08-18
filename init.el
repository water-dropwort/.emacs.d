;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)
    ))
;; </leaf-install-code>

;; デフォルトでセットされているキーバインドを解除する。
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-\\"))

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
;; Alt+上下キーで行を移動できるようにする。
(defun move-line (direction)
  (let ((col (current-column)))
    (unless (eq col 0)
      (move-to-column 0))
    (save-excursion
      (forward-line)
      (transpose-lines direction))
    (forward-line direction)))
(defun move-line-down ()
  (interactive)
  (move-line 1))
(defun move-line-up ()
  (interactive)
  (move-line -1))
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<up>") 'move-line-up)
;; バッファ移動キーバインド
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
;; バッファ入れ替えキーバインド
(global-set-key (kbd "C-c S-<left>") 'windmove-swap-states-left)
(global-set-key (kbd "C-c S-<down>") 'windmove-swap-states-down)
(global-set-key (kbd "C-c S-<up>") 'windmove-swap-states-up)
(global-set-key (kbd "C-c S-<right>") 'windmove-swap-states-right)
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
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode(built-in)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; 存在するファイルを開くときはview-modeで開くようにする。
(defun my/find-file-setup ()
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (not (buffer-modified-p)))
    (view-mode)))
(add-hook 'find-file-hook 'my/find-file-setup)

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

(leaf company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 5)
  (add-hook 'prog-mode-hook 'company-mode)
  :defer-config
  (set-face-attribute 'company-tooltip-selection nil :box t)
  )

(leaf eglot
  :ensure t
  :init
  ;; compat(Verticoの依存パッケージ)でrequire-wtih-checkが定義され、
  ;; eglotの初期化処理がうまく動作しない。
  (leaf compat
    :init (fset 'require-with-check nil))
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(leaf dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(file-size subtree-state))
  (define-key dirvish-mode-map (kbd "TAB")       'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "<backtab>") 'dirvish-subtree-remove))

(leaf rainbow-delimiters
  :ensure t
  :require t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (set-face-attribute 'rainbow-delimiters-base-error-face nil :foreground "pink"))

(leaf yaml-mode
  :ensure t)

;; (leaf platformio-mode
;;   :ensure nil
;;   :init
;;   (leaf projectile :ensure t)
;;   (leaf async :ensure t)
;;   :el-get water-dropwort/PlatformIO-Mode
;;   :config
;;   (add-to-list 'projectile-project-root-files "platformio.ini")
;;   :setq
;;   (path-to-platformio . "$HOME/.local/bin/platformio")
;;   :hook
;;   ((c++-mode-hook c-mode-hook) . platformio-conditionally-enable))

;; (leaf consult
;;   :ensure nil
;;   :el-get water-dropwort/consult
;;   :bind
;;   ("C-c g" . consult-grep)
;;   ("C-c l" . consult-line)
;;   ("C-c i" . consult-imenu)
;;   )

;; ;; Existing buffers will is enabled view-mode by default at first.
;; (defun my/find-file-setup ()
;;   (when (and (buffer-file-name)
;;              (file-exists-p (buffer-file-name))
;;              (not (buffer-modified-p)))
;;     (view-mode)))
;; (add-hook 'find-file-hook 'my/find-file-setup)

;; (leaf csharp-mode
;;   :ensure t)

;; (leaf desktop/tool-bar/tab-bar
;;   ;; Tabs are not displayed at startup.
;;   ;; It seems to be influenced by desktop-save,tool-bar and i3wm.
;;   ;; Setting these when window-setup-hook is fired appears to resolve the issue.
;;   :init
;;   (leaf tab-bar
;;     :require t
;;     :custom-face
;;     (tab-bar-tab
;;      . '((t (:foreground "black" :background "#77d9a8"))))
;;     (tab-bar-tab-inactive
;;      . '((t (:foreground "white" :background "dimgray")))))
;;   (leaf desktop
;;     :require t
;;     :config
;;     (defun my/time-subtract-days (time1 time2)
;;       (let ((sub-sec (time-convert (time-subtract time1 time2) 'integer)))
;;         (/ sub-sec 60 60 24)))
;;     (defvar my/emacs-start-time (current-time))
;;     (defun my/desktop-get-var-from-local-variables (vars varname)
;;       (let ((ret))
;;         (dolist (var vars)
;;           (let ((name (car var))
;;                 (val  (cdr var)))
;;             (when (eq name varname)
;;               (setq ret val))))
;;         ret))
;;     (defun my/desktop-buffers-not-to-save-functions (filename bufname mode rest)
;;       (let* ((local-vars (nth 5 rest))
;;              (display-time (my/desktop-get-var-from-local-variables
;;                             local-vars 'buffer-display-time)))
;;         (> (my/time-subtract-days display-time my/emacs-start-time) -14)))
;;     (add-to-list 'desktop-modes-not-to-save 'dired-mode))
;;   :hook
;;   ;;(window-setup-hook . (lambda ()
;;   ;;                       (progn
;;   ;;                         (desktop-save-mode t)
;;   ;;                         (desktop-read)
;;   ;;                         (tool-bar-mode -1)
;;   ;;                         (tab-bar-mode t))))
;;   )

(provide 'init)
