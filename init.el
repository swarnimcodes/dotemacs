;; Stop emacs from littering config file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable UI elements early in startup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Basic settings
(setq-default show-trailing-whitespace t)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-18"))
(set-frame-font "Iosevka Nerd Font-18" nil t)
(setq inhibit-startup-screen t)
(setq-default pixel-scroll-precision-mode t)
(setq-default line-spacing 0.12)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq js-switch-indent-offset 2)

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Line movement functions
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<up>")  'move-line-up)
(global-set-key (kbd "M-<down>")  'move-line-down)

;; Install and configure packages
(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config)
;;   )

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(use-package multiple-cursors
  :ensure t
  :bind (:map global-map
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         )
  )

(use-package eat
  :ensure t)

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode t))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'prog-mode-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'typescript))

;; Languages
(use-package go-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(setq treesit-language-source-alist '((c3 "https://github.com/c3lang/tree-sitter-c3")))
(add-to-list 'load-path "~/.emacs.d/extras/")
(require 'c3-ts-mode)
(setq treesit-font-lock-level 4)

;; LSP configuration
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp-deferred))
  :commands lsp-deferred
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  (lsp-log-io nil)
  (lsp-warn-no-matched-client nil)
  ;; Completion related settings
  (lsp-completion-provider :none)
  (lsp-completion-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-enable-folding nil)
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers nil)
  :config
  ;; Configure LSP completion
  (setq lsp-completion-enable-additional-text-edit nil)
  ;; Add completion to LSP completion list
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults)
                    '((styles . (orderless))))))
  )

;; Enable LSP UI features for documentation
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)  ; Enable documentation on hover
  (lsp-ui-doc-show-with-cursor nil)  ; Show doc when cursor is on symbol
  (lsp-ui-doc-position 'at-point)  ; Show doc at point (alternatively 'top' or 'bottom')
  (lsp-ui-doc-delay 0.2)  ; Small delay before showing documentation
  (lsp-ui-doc-max-height 30)  ; Maximum height of doc window
  (lsp-ui-sideline-enable nil)  ; Enable sideline information
  (lsp-ui-sideline-show-hover nil)  ; Show hover information in sideline
  (lsp-ui-sideline-show-diagnostics nil)  ; Show diagnostics in sideline
  (lsp-ui-sideline-show-code-actions nil))  ; Show code actions in sideline

;; Performance optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
