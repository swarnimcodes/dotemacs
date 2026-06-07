;;; init.el --- init file -*- lexical-binding: t; -*-

;;; Code:

;; Font
(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font-16"))

(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)  ;; Relative line numbers
(setq show-trailing-whitespace 1)
(setq whitespace-style '(face tabs spaces trailing tab-mark space-mark))
(global-whitespace-mode 1)
(setq make-backup-files nil)
(setq create-lockfiles nil)
;; Some ELPA packages emit harmless async native-comp warnings on Emacs 30
;; for optional integrations they probe at compile time.
(setq native-comp-async-report-warnings-errors nil)

;; Package Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)


;; c3 mode
(load-file (expand-file-name "simpc3-mode.el" user-emacs-directory))


;; envrc
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; theme
(use-package gruber-darker-theme)
(load-theme 'gruber-darker t)

;; turepo
(use-package turepo
  :ensure t
  :bind (("C-c g r" . turepo)))

;; magit
(use-package magit
  :commands (magit-status))

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

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

;; corfu - in-buffer completion
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt))

;; cape - completion-at-point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; auto treesitter config
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)  ; Prompt to install missing grammars
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
  (add-to-list 'apheleia-formatters
               '(c3fmt . ("c3fmt" "--stdin" "--stdout"
                          "--stdin-filepath" filepath)))
  (add-to-list 'apheleia-mode-alist '(simpc3-mode . c3fmt))
  (apheleia-global-mode +1))


;; languages
(use-package typescript-mode)
(use-package go-mode)
(use-package rust-mode)
(use-package markdown-mode)


(use-package yasnippet
  :config
  (yas-global-mode 1))

;; lsp-mode + lsp-ui
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable t
        lsp-completion-provider :none)) ;; Use completion-at-point (corfu/cape)

;; Add lsp completion to cape
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'cape
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t))


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
   '(apheleia cape consult corfu envrc expand-region git-gutter go-mode
	      gruber-darker-theme lsp-ui magit marginalia
	      multiple-cursors orderless rust-mode treesit-auto turepo
	      typescript-mode vertico yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
