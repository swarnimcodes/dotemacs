;;; init.el --- init file -*- lexical-binding: t; -*-

;;; Code:

;; Font
(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font-18"))

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
;; (load-file (expand-file-name "simpc3-mode.el" user-emacs-directory))

;; modeline
(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; collapse the minor-mode list (yas, WK, ...) into one menu
(use-package minions
  :config
  (setq minions-mode-line-lighter "…")
  (minions-mode 1))

;; typst
(use-package typst-ts-mode
  :ensure t
  :mode "\\.typ\\'")

;; envrc
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; theme
;; (use-package gruber-darker-theme)
;; (load-theme 'gruber-darker t)
(use-package ef-themes)
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" user-emacs-directory))
(load-theme 'ef-tokyo-night t)

;; turepo
(use-package turepo
  :ensure t
  :bind (("C-c g r" . turepo)))

;; magit
(use-package magit
  :commands (magit-status))

;; diff-hl
(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  :hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; pdf-tools - PDF viewer (first use compiles the epdfinfo server)
(use-package pdf-tools
  :config
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  ;; live update: reload the PDF whenever it changes on disk
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode)
  (setq auto-revert-interval 0.5))

;; mistty - terminal with Emacs-style command-line editing
(use-package mistty
  :bind (("C-c s" . mistty)))

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
   '(apheleia consult envrc expand-region git-gutter go-mode
	      gruber-darker-theme magit marginalia moody
	      multiple-cursors orderless rust-mode treesit-auto turepo
	      typescript-mode typst-ts-mode vertico yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
