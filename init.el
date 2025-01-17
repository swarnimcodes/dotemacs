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
(setq dired-dwim-target t)
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

;;  Custom Keybindings
(global-set-key (kbd "C-`") 'other-window)

(global-set-key (kbd "M-m") 'compile)
(global-set-key (kbd "M-i") 'back-to-indentation)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(global-set-key (kbd "M-<up>")  'move-line-up)
(global-set-key (kbd "M-<down>")  'move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install and configure packages ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (tsx-ts-mode . lsp-deferred)
  (jsx-ts-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  ;; Fallback modes
  (typescript-mode . lsp-deferred)
  (js-mode . lsp-deferred)

  (js-ts-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package apheleia
  :ensure t)
(apheleia-global-mode +1)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package org
  :ensure t
  :config
  ;; Basic settings
  (setq org-startup-indented t)           ; Enable org-indent-mode by default
  (setq org-startup-folded 'overview)     ; Start files folded
  (setq org-return-follows-link t)        ; Make RET follow links
  (setq org-hide-emphasis-markers t)      ; Hide formatting characters
  (setq org-pretty-entities t)            ; Show entities as UTF8 characters
  )

;; (use-package gruber-darker-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruber-darker t))

;; (load-theme 'modus-vivendi)

(use-package nerd-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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

;; Install Magit from main branch
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

;; Languages
(use-package go-mode
  :ensure t)

(use-package typescript-mode
  :after lsp-mode
  :config
  (add-to-list 'lsp-disabled-clients '(typescript-mode . typescript-language-server))
  (add-to-list 'lsp-enabled-clients 'ts-ls))

;; Performance optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
