;; emacsのメジャーバージョンは26.1


;; フレームの設定                              
(setq initial-frame-alist                      
      (append (list                            
;;               '(alpha . (95 5))             
;;               '(width . 120)                
;;               '(height . 70)                
               '(width . 120)                   
               '(height . 56)                  
               '(top . 0)                     
               '(left . 0))                    
              initial-frame-alist))            
(setq default-frame-alist initial-frame-alist) 



;; load-pathを追加する関数を定義

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; インデントにタブ文字を利用する
(setq-default indent-tabs-mode nil)
;; 特定のモードの場合はインデントにタブ文字を利用しない
(add-hook 'emacs-lisp-mode-hook (function (lambda () (setq indent-tabs-mode nil))))
(add-hook 'org-mode-hook (function (lambda () (setq indent-tabs-mode nil))))

;; インデントを 4 に設定
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq php-indent-level 2)
(setq cperl-indent-level 4)
(setq cperl-continued-statement-offset 4)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-region-fix-constructs 1)
(setq cperl-indent-parens-as-block t)
(setq sgml-basic-offset 4)
(setq ruby-indent-tabs-mode nil)
(setq ruby-indent-size 2)
(setq ruby-indent-level 2)
(setq coffee-tab-width 2)

;; タブ幅を 4 に設定
(setq tab-width 4)
(setq-default tab-width 4)
;; (setq tab-stop-list (let ((stops '(4)))
;;                       (while (< (car stops) 120)
;;                         (setq stops (cons (+ 4 (car stops)) stops)))
;;                       (nreverse stops)))

;; 自動セーブとバックアップファイルを無効にする
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

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

;; trampにてps1をカスタマイズしてるサーバに接続する為の設定
(setq tramp-shell-prompt-pattern "\\(?:^\\|\^M\\)[^#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)* *")

;; trampにて履歴ファイルを保存しない
(setq tramp-histfile-override "/dev/null")

;; themesディレクトリを追加
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; theme

(load-theme 'iceberg t)
(enable-theme 'iceberg)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (inf-ruby rbenv ruby-block ruby-electric ruby-mode helm php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; ドラッグドロップでファイルを開く
(define-key global-map [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)


;; scratchバッファの制御
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

;; commentの設定
(defconst comment-styles
  '((plain	. (nil nil nil nil))
    (indent	. (nil nil nil t))
    (indent-or-triple
     . (nil nil nil multi-char))
    (aligned	. (nil t nil t))
    (multi-line	. (t nil nil t))
    (extra-line	. (t nil t t))
    (box	. (nil t t t))
    (box-multi	. (t t t t)))
  "Comment region styles of the form (STYLE . (MULTI ALIGN EXTRA INDENT)).
STYLE should be a mnemonic symbol.
MULTI specifies that comments are allowed to span multiple lines.
ALIGN specifies that the `comment-end' markers should be aligned.
EXTRA specifies that an extra line should be used before and after the
  region to comment (to put the `comment-end' and `comment-start').
INDENT specifies that the `comment-start' markers should not be put at the
  left margin but at the current indentation of the region to comment.
If INDENT is `multi-char', that means indent multi-character
  comment starters, but not one-character comment starters.")
;; comment-style (indent, multi-line, box)
(setq comment-style 'box)

;; yankのシステムへのコピー
(cond (window-system (setq x-select-enable-clipboard t)))

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])
;;(define-key html-mode-map [?¥] [?\\])
(define-key isearch-mode-map [?¥] '(lambda () (interactive)(isearch-process-search-char ?\\)))


;; ----------------------------------------以下拡張周りの設定--------------------------------------------


(require 'package) ; package.elを有効化
;; パッケージリポジトリにMarmaladeとMelpaを追加
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;;php-mode
(require 'php-mode)
;; PHPにて特定のインデントを変更
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-offset 'substatement-open 0)
            ))

;; helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode t)

(helm-mode 1)

;;重くなるのでhelm-for-filesだけハイライトを無効化する
(defadvice helm-for-files
  (around helm-for-files-no-highlight activate)
  "No highlight when using helm-for-files."
  (let ((helm-mp-highlight-delay nil))
    ad-do-it))

;; helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)
;; fuzzy matching of helm-M-x
(setq helm-M-x-fuzzy-match t) ;; optional
;; helm-show-kill-ring 履歴管理
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-;") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; 使いにくいのでコメントアウト
(global-set-key (kbd "C-x C-o") 'helm-find-files)
;; helm-occur
(global-set-key (kbd "C-c o") 'helm-occur)


;; settings about ruby
(setenv "RBENV_ROOT" "~/.anyenv/envs/rbenv")
(require 'rbenv)
(global-rbenv-mode)

(require 'ruby-block)
(ruby-block-mode t)

; ruby-block-delay を0.50 → 0に設定
(defcustom ruby-block-delay 0
  "*Time in seconds to delay before showing a matching paren."
  :type  'number
  :group 'ruby-block)

;; ruby-mode等
(add-to-list 'load-path "~/.emacs.d/elisp/ruby-mode")
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
;;(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;;ruby-electric.el
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; set ruby-mode indent
(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)


;; 入力されるキーシーケンスを入れ換える
;; ?\C-?はDELのキーシーケンス
(keyboard-translate ?\C-h ?\C-?)
;; C-h
;;(global-set-key (kbd "C-h") 'delete-backward-char)
;; 別のキーバインドにヘルプを割り当てる
(global-set-key (kbd "C-x ?") 'help-command)


;;; リージョンを削除できるように
(delete-selection-mode t)

