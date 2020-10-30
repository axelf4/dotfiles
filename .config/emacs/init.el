;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Bootstrap the straight.el package manager
(load (expand-file-name "straight/repos/straight.el/bootstrap.el"
						user-emacs-directory) nil 'nomessage)

(setq gc-cons-threshold 16777216
	  enable-recursive-minibuffers t
	  scroll-conservatively most-positive-fixnum ; Do not center cursor after scrolling
	  truncate-partial-width-windows nil ; Always soft-wrap
	  show-paren-delay 0
	  sentence-end-double-space nil ; Single space between sentences
	  make-backup-files nil
	  auto-save-no-message t
	  vc-handled-backends nil) ; Disable VC
(setq-default tab-width 4)
(menu-bar-mode -1) ; Disable the menu bar
(show-paren-mode 1) ; Highlight matching parentheses
(electric-pair-mode) ; Auto-pairing
(global-set-key [escape] 'keyboard-escape-quit)
(fset 'yes-or-no-p 'y-or-n-p)

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

;; vi emulation
(straight-use-package 'evil)
(straight-use-package 'goto-chg)
(straight-use-package 'evil-numbers)
(setq
 ;; Behave more like Vim
 evil-want-C-u-delete t
 evil-want-C-u-scroll t
 evil-search-module 'evil-search
 evil-ex-search-highlight-all nil ; No hlsearch
 evil-toggle-key "" ; Do not map CTRL-Z

 evil-undo-system 'undo-tree
 evil-want-Y-yank-to-eol t ; Make Y consistent with other capitals
 evil-split-window-below t evil-vsplit-window-right t)

(evil-mode 1)
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global
  (kbd "C-a") 'evil-numbers/inc-at-pt
  (kbd "C-x") 'evil-numbers/dec-at-pt)

;; c_CTRL-U
(define-key minibuffer-local-map
  (kbd "C-u") 'evil-delete-back-to-indentation)

(evil-set-initial-state 'help-mode 'normal)
(evil-define-key 'normal help-mode-map (kbd "C-t") 'help-go-back)

;; Fuzzy finding
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(selectrum-mode 1)
(selectrum-prescient-mode 1)

;; Project management
(straight-use-package 'projectile)
(projectile-mode 1)
(evil-define-key 'normal projectile-mode-map
  (kbd "<leader>p") 'projectile-command-map)

;; Customize mode line
(setq-default
 mode-line-buffer-identification
 '(:eval
   (if (buffer-file-name)
	   (let* ((dir (file-name-directory buffer-file-name))
			  (dir (if-let (project-root (projectile-project-root))
					   (string-remove-prefix project-root dir)
					 ;; Abbreviate $HOME
					 (replace-regexp-in-string
					  (concat "\\`" (regexp-quote (expand-file-name "~"))) "~" dir)))
			  (parts (split-string dir "/" nil)))
		 (list
		  (if (> (length parts) 3)
			  (string-join `(,(car parts) "..." . ,(last parts 2)) "/")
			dir)
		  (propertize (file-name-nondirectory (buffer-file-name))
					  'face 'mode-line-buffer-id)))
	 (propertize (buffer-name) 'face 'mode-line-buffer-id))))
(setq-default
 mode-line-format
 '("%e " ; Out-of-memory indication
   mode-line-buffer-identification
   (:eval (when (buffer-modified-p) " [+]"))
   ;; Right-justified line and column number
   (:eval (let* ((rhs (format-mode-line "%l,%C "))
				 (hpos (- (window-width) (string-width rhs))))
			(list (propertize " " 'display `(space :align-to ,hpos)) rhs)))
   ))

;; Colorscheme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox t)
