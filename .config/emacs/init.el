;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Bootstrap the straight.el package manager
(load (expand-file-name "straight/repos/straight.el/bootstrap.el"
						user-emacs-directory) nil 'nomessage)

(setq gc-cons-threshold 16777216
	  enable-recursive-minibuffers t
	  scroll-conservatively most-positive-fixnum ; Do not center cursor after scrolling
	  truncate-partial-width-windows nil ; Always soft-wrap
	  select-enable-clipboard nil ; Do not tie unnamed register "" to system clipboard
	  sentence-end-double-space nil ; Single space between sentences
	  show-paren-delay 0
	  make-backup-files nil
	  auto-save-no-message t
	  vc-handled-backends nil) ; Disable VC
(setq-default tab-width 4)
(menu-bar-mode -1) ; Disable the menu bar
(show-paren-mode 1) ; Highlight matching parentheses
(global-set-key [escape] 'keyboard-escape-quit)
(fset 'yes-or-no-p 'y-or-n-p)

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

(electric-pair-mode) ; Autopairing
;; Inhibit autopairing in minibuffers
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local electric-pair-inhibit-predicate 'identity)))

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
 evil-ex-substitute-case 'sensitive
 evil-toggle-key "" ; Do not map CTRL-Z
 glc-default-span 1 ; Consider only immediately adjacent changes as the same

 evil-undo-system 'undo-tree
 evil-want-Y-yank-to-eol t ; Make Y consistent with other capitals
 evil-split-window-below t evil-vsplit-window-right t)

(evil-mode 1)
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global
  (kbd "C-a") 'evil-numbers/inc-at-pt
  (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Inherit command-line mappings in minibuffers
(set-keymap-parent minibuffer-local-map evil-ex-completion-map)
(define-key minibuffer-local-map [remap completion-at-point] nil) ; but undo remapping...
;; Make Evil motions/text objects always use the default paragraph definition
(advice-add 'forward-evil-paragraph :around
            (lambda (orig-fun &rest args)
              (let ((paragraph-start (default-value 'paragraph-start))
                    (paragraph-separate (default-value 'paragraph-separate)))
                (apply orig-fun args))))
(define-key evil-normal-state-map [remap goto-last-change]
  (lambda (arg)
    "Go to penultimate change with cursor already on the last."
    (interactive "P")
    (setq this-command 'goto-last-change)
    (let ((old-pos (point)))
          (goto-last-change arg)
          (when (<= (abs (- old-pos (point))) glc-default-span)
            (setq last-command this-command)
            (goto-last-change arg)))))

(evil-set-initial-state 'help-mode 'normal)
(evil-define-key 'normal help-mode-map (kbd "C-t") 'help-go-back)
(define-key key-translation-map (kbd "<leader>h") "\C-h")

;; System clipboard support while running in terminal
(straight-use-package 'xclip)
(ignore-error file-error ; Silence missing backend error
  (xclip-mode))
(defun save-kill-ring (fun &rest args)
  "Call FUN with ARGS and restore the kill ring afterward."
  (let ((kill-ring kill-ring)) (apply fun args)))
;; Pasting from system clipboard should not also copy the pasted text
(advice-add 'current-kill :around #'save-kill-ring)

;; Fuzzy finding
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(selectrum-mode 1)
(selectrum-prescient-mode 1)
(setq selectrum-extend-current-candidate-highlight t)

;; Project management
(set-frame-parameter nil 'cwd default-directory) ; For the initial frame
(push 'cwd frame-inherited-parameters)
(defun project-root ()
  "Get the frame-local current working directory."
  (frame-parameter nil 'cwd))
(add-hook
 'server-after-make-frame-hook
 (lambda ()
   (let ((client (frame-parameter nil 'client)))
     (set-frame-parameter nil 'cwd (process-get client 'server-client-directory))
     ;; find-file-hook for files visited by client runs before frame creation
     (dolist (buf (process-get client 'buffers))
       (with-current-buffer buf (setq default-directory (project-root)))))))
(add-hook 'find-file-hook (lambda () (setq default-directory (project-root))))

(let ((find-files-program (cond
                           ((executable-find "rg") '("rg" "--color=never" "--files"))
                           ((executable-find "find") '("find" "-type" "f")))))
  (defun find-file-rec ()
    "Find a file in the current working directory recursively."
    (interactive)
    (let ((default-directory (project-root)))
      (find-file
       (completing-read
        "Find file: " (apply 'process-lines find-files-program))))))

(evil-define-key 'normal 'global
  (kbd "<leader>f") 'find-file-rec)

;; Customize mode line
(setq-default
 mode-line-buffer-identification
 '(:eval
   (if (buffer-file-name)
       (let* ((dir (abbreviate-file-name
                    (string-remove-prefix (project-root)
                                          (file-name-directory buffer-file-name))))
              (parts (split-string dir "/")))
         (list
          (if (< (length parts) 5) dir
            (string-join `(,(car parts) "..." . ,(last parts 2)) "/"))
          (propertize (file-name-nondirectory (buffer-file-name))
                      'face 'mode-line-buffer-id)))
     (propertize (buffer-name) 'face 'mode-line-buffer-id)))
 mode-line-modified '(:eval (when (buffer-modified-p) " [+]"))
 mode-line-format
 '("%e " ; Out-of-memory indication
   mode-line-buffer-identification mode-line-modified
   ;; Right-justified line and column number
   (:eval (let* ((rhs (format-mode-line "%l,%C"))
                 (hpos (- (window-width) (string-width rhs) 1)))
            (list (propertize " " 'display `(space :align-to ,hpos)) rhs)))))

;; Colorscheme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox t)

;; Language support
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(straight-use-package 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
		  (lambda () (setq indent-tabs-mode nil)))

(straight-use-package 'haskell-mode)

(straight-use-package 'nix-mode)
