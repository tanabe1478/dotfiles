;; emacsのメジャーバージョンは26.1


;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")



;; Mac の文字コードの設定
(set-language-environment "Japanese")
(require 'ucs-normalize)
;; (prefer-coding-system 'utf-8-hfs)
;; (setq file-name-coding-system 'utf-8-hfs)
;; (setq locale-coding-system 'utf-8-hfs)
;; utf-8-hfs の濁点付カナがapacheで認識出来なかった為以下で対応
(prefer-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-unix)

;;;; 半角と全角を1:2に
;;(setq face-font-rescale-alist
;;      '((".*Menlo.*" . 1.0)
;;        (".*Hiragino_Mincho_ProN" . 1.2)
;;        ;; (".*nfmotoyacedar-bold.*" . 1.2)
;;        ;; (".*nfmotoyacedar-medinum.*" . 1.2)
;;        ("-cdac$" . 1.3)))

;; フォントの設定
(if (window-system)
    (progn
      (set-face-attribute 'default nil
                          :family "Ricty"
                          :height 140)
      (set-fontset-font
       nil 'japanese-jisx0208
       (font-spec :family "Ricty"))
      (set-fontset-font
       nil 'katakana-jisx0201
       (font-spec :family "Ricty"))
      )
  )


;; フレームの設定
(setq initial-frame-alist
      (append (list
;;               '(alpha . (95 5))
;;               '(width . 120)
;;               '(height . 70)
               '(width . 99)
               '(height . 56)
               '(top . 0)
               '(left . 0))
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)
;; tool-bar を非表示。コンソールでは不要
(tool-bar-mode 0)
;; scroll-bar を非表示。コンソールでは不要
 (scroll-bar-mode 0)
;; menu-bar を非表示
;; (menu-bar-mode 0)

;; メニューバーにファイルパスを表示する
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

(set-face-attribute 'show-paren-match nil
      :background 'unspecified
      :underline "turquoise")

;; 行番号を表示する
(global-linum-mode t)

;; カーソルの位置を表示する
(line-number-mode t)
(column-number-mode t)

;; バッファ名をディレクトリ名に変更
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; uniquify でバッファ名を変更しないものを正規表現で指定
(setq uniquify-ignore-buffers-re "*[^*]+*")
;; ファイル名が重複してない場合も常にディレクトリ名を表示するよう指定
(setq uniquify-min-dir-content 1)

;; Mac の command キーと Alt キーを入れ換える
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; C-m に newline-and-indent を割り当てる。初期値は newline
;; (define-key global-map (kbd "C-m") 'newline-and-indent)

;; "M-k" でカレントバッファを閉じる。初期値は kill-sentence
(define-key global-map (kbd "M-k") 'kill-this-buffer)

;; "C-t" でウィンドウを切り替える。初期値は transpose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; "C-sC-w" でカーソル下の単語を検索
(defun isearch-forward-with-heading ()
  "Search the word your cursor looking at."
  (interactive)
  (command-execute 'backward-word)
  (command-execute 'isearch-forward))
(global-set-key "\C-s" 'isearch-forward-with-heading)

;; 入力されるキーシーケンスを入れ換える
;; ?\C-?はDELのキーシーケンス
(keyboard-translate ?\C-h ?\C-?)
;; 別のキーバインドにヘルプを割り当てる
(global-set-key (kbd "C-x ?") 'help-command)

;; trampにてps1をカスタマイズしてるサーバに接続する為の設定
(setq tramp-shell-prompt-pattern "\\(?:^\\|\^M\\)[^#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)* *")

;; trampにて履歴ファイルを保存しない
(setq tramp-histfile-override "/dev/null")

;; theme
(load-theme 'deeper-blue' t)

