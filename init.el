;;; init.el --- init file -*- lexical-binding: t; -*-

;;; Code:

(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq show-trailing-whitespace 1)

;; Package Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

;; theme
(use-package gruber-darker-theme)
(load-theme 'gruber-darker t)


;; magit
(use-package magit
  :commands (magit-status))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; consult
(use-package consult)

;; vertico
(use-package vertico
  :init
  (vertico-mode))

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode t))

;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; auto treesitter config
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; apheleia
(use-package apheleia
  :config
  (apheleia-global-mode +1))


;; languages
(use-package typescript-mode)
(use-package go-mode)

(use-package markdown-mode)
(use-package yasnippet)

;; lsp
(add-to-list 'load-path "~/.emacs.d/lsp-bridge")

(setq lsp-bridge-python-command "~/.emacs.d/lsp-bridge-env/bin/python")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-breadcrumb-mode t)

(with-eval-after-load 'lsp-bridge
  (define-key lsp-bridge-mode-map (kbd "C-c l k") 'lsp-bridge-popup-documentation)
  (define-key lsp-bridge-mode-map (kbd "C-c l r") 'lsp-bridge-rename)
  (define-key lsp-bridge-mode-map (kbd "C-c l d") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-c l a") 'lsp-bridge-code-action)
  (define-key lsp-bridge-mode-map (kbd "C-c l f") 'lsp-bridge-find-references)
  )


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

;; Global keybindings
(global-set-key (kbd "C-`") 'other-window)
(global-set-key (kbd "M-m") 'compile)
(global-set-key (kbd "M-i") 'back-to-indentation)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)



(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia consult expand-region go-mode gruber-darker
	      gruber-darker-theme magit marginalia markdown-mode
	      multiple-cursors orderless treesit-auto typescript-mode
	      vertico yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
