(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(set-frame-font "Iosevka-25")
(setq inhibit-startup-screen 1)
(setq-default pixel-scroll-precision-mode t)
(setq-default line-spacing 0.12)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
(dolist (package '(vertico marginalia orderless gruber-darker-theme
                           tree-sitter tree-sitter-langs
                           consult lsp-mode
                           ))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; Consult configuration
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-c f") 'consult-find)
(global-set-key (kbd "C-c g") 'consult-ripgrep)


(vertico-mode t)
(marginalia-mode t)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(load-theme 'gruber-darker t)
