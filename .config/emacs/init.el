;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

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
      tags-revert-without-query t
      vc-handled-backends nil) ; Disable VC
(setq-default tab-width 4)
(menu-bar-mode 0) ; Disable the menu bar
(show-paren-mode) ; Highlight matching parentheses
(global-auto-revert-mode)
(global-set-key [escape] 'keyboard-escape-quit)
(fset 'yes-or-no-p 'y-or-n-p)

(electric-pair-mode) ; Autopairing
;; Inhibit autopairing in minibuffers
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local electric-pair-inhibit-predicate #'identity)))

;;; vi emulation
(straight-use-package 'evil)
(straight-use-package 'undo-tree)
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
 evil-symbol-word-search t
 evil-split-window-below t evil-vsplit-window-right t
 ;; Default fails to mimic Vim by not wrapping before looking in other buffers
 evil-complete-next-func #'dabbrev-expand)
(evil-mode)
(evil-set-leader 'motion (kbd "SPC"))
(add-hook 'evil-local-mode-hook #'undo-tree-mode)
(evil-define-key 'normal 'global
  "\C-a" 'evil-numbers/inc-at-pt "\C-x" 'evil-numbers/dec-at-pt
  (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental
  (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental)
;; Bindings that make motion state usable as initial state for read-only modes
(evil-define-key 'motion 'global "ZZ" 'evil-save-modified-and-close "ZQ" 'evil-quit)

;; Inherit command-line mappings in minibuffers
(set-keymap-parent minibuffer-local-map evil-ex-completion-map)
(evil-define-key nil minibuffer-local-map ; but undo remappings...
  [remap completion-at-point] nil
  "\C-n" 'next-line "\C-p" 'previous-line)
;; Always use blank lines as paragraph delimiters in motions/text objects
(advice-add #'forward-evil-paragraph :around
            (lambda (orig-fun &rest args)
              (let ((paragraph-start (default-value 'paragraph-start))
                    (paragraph-separate (default-value 'paragraph-separate))
                    (paragraph-ignore-fill-prefix t))
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

(evil-set-initial-state 'messages-buffer-mode 'motion)
;; ...and the preexistent "*Messages*" buffer
(with-current-buffer (messages-buffer) (evil-motion-state))

(global-set-key (kbd "<leader>h") 'help-command)
(evil-define-key 'motion help-mode-map "\C-t" 'help-go-back)

;; System clipboard support while running in terminal
(straight-use-package 'xclip)
(ignore-error file-error ; Silence missing backend error
  (xclip-mode))
(defun save-kill-ring (fun &rest args)
  "Call FUN with ARGS and restore the kill ring afterward."
  (let ((kill-ring kill-ring)) (apply fun args)))
;; Pasting from system clipboard should not also copy the pasted text
(advice-add #'current-kill :around #'save-kill-ring)

;;; Minibuffer completion
(straight-use-package 'selectrum)
(straight-use-package 'hotfuzz)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      selectrum-extend-current-candidate-highlight t)
(selectrum-mode)
(hotfuzz-selectrum-mode)

;;; Project management
(set-frame-parameter nil 'cwd default-directory) ; For the initial frame
(push 'cwd frame-inherited-parameters)
(defun project-root ()
  "Get the frame-local current working directory."
  (frame-parameter nil 'cwd))
(defun cd-to-project ()
  "Make the project root the current buffer's default directory."
  ;; Magit relies on the original default-directory value
  (unless (and buffer-file-name
               (equal (file-name-nondirectory buffer-file-name) "COMMIT_EDITMSG"))
    (setq default-directory (project-root))))
(add-hook
 'server-after-make-frame-hook
 (lambda ()
   (when-let ((client (frame-parameter nil 'client)))
     (set-frame-parameter nil 'cwd (process-get client 'server-client-directory))
     ;; find-file-hook for files visited by client runs before frame creation
     (dolist (buf (process-get client 'buffers))
       (with-current-buffer buf (cd-to-project))))))
(add-hook 'find-file-hook #'cd-to-project)

(let ((find-files-program (cond
                           ((executable-find "rg") '("rg" "--color=never" "--files"))
                           ((executable-find "find") '("find" "-type" "f")))))
  (defun find-file-rec ()
    "Find a file in the current working directory recursively."
    (interactive)
    (let ((default-directory (project-root)))
      (find-file
       (completing-read "Find file: "
                        (apply #'process-lines find-files-program))))))

;;; Customize mode line
(setq-default
 mode-line-buffer-identification
 '(:eval
   (if buffer-file-name
       (let* ((dir (abbreviate-file-name
                    (string-remove-prefix (expand-file-name (project-root))
                                          (file-name-directory buffer-file-name))))
              (parts (split-string dir "/")))
         (list
          (if (< (length parts) 5) dir
            (string-join `(,(car parts) "..." . ,(last parts 2)) "/"))
          (propertize (file-name-nondirectory buffer-file-name)
                      'face 'mode-line-buffer-id)))
     (propertize (buffer-name) 'face 'mode-line-buffer-id)))
 mode-line-modified '(:eval (when (buffer-modified-p) " [+]"))
 mode-line-percent-position '(-3 "%o")
 mode-line-position '("%l,%C " mode-line-percent-position)
 mode-line-format
 '("%e " ; Out-of-memory indication
   mode-line-buffer-identification mode-line-modified
   ;; Right-justified ruler
   (:eval (let* ((rhs (format-mode-line mode-line-position))
                 (hpos (- (window-width) (string-width rhs) 1)))
            (propertize " " 'display `(space :align-to ,hpos))))
   mode-line-position))

;;; Compilation
(straight-use-package 'xterm-color)
(setq compilation-scroll-output t
      compilation-ask-about-save nil ; Save before compilation
      ;; Make compilations unique per frame
      compilation-buffer-name-function
      (lambda (name-of-mode)
        (concat "*" (downcase name-of-mode) "-" (frame-parameter nil 'name) "*"))
      compilation-environment '("TERM=xterm-256color"))
;; Interpret ANSI escape sequences in compilation output
(advice-add #'compilation-filter :filter-args
            (cl-function (lambda ((proc string))
                           (list proc (xterm-color-filter string)))))
(advice-add
 #'compilation-start :around
 (lambda (fun &rest args)
   (let ((compilation-environment (append compilation-environment
                                          (frame-parameter nil 'environment))))
     (apply fun args))))
(defun compile-or-recompile ()
  "Redo a previous compilation if such exists or prompt for a command.
Unlike `recompile' it is not necessary to run this in the Compilation
mode buffer."
  (interactive)
  (require 'compile)
  (if-let ((buf (get-buffer (compilation-buffer-name "compilation"
                                                     'compilation-mode
                                                     nil))))
      (with-current-buffer buf (recompile))
    (call-interactively #'compile)))

;;; Colorscheme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox t)

;;; Magit
(straight-use-package 'magit)
(setq git-commit-summary-max-length 50
      git-commit-fill-column 72)
(with-eval-after-load 'transient
  (define-key transient-base-map [escape] 'transient-quit-one)
  (define-key transient-sticky-map [escape] 'transient-quit-seq))
(dolist (mode '(magit-status-mode
                magit-log-mode
                magit-log-select-mode
                magit-revision-mode
                magit-diff-mode
                magit-process-mode
                magit-stashes-mode))
  (evil-set-initial-state mode 'motion))
(evil-set-initial-state 'git-rebase-mode 'normal)
(evil-define-key 'motion magit-mode-map
  "gr" 'magit-refresh "gR" 'magit-refresh-all
  [escape] 'magit-mode-bury-buffer
  (kbd "RET") 'magit-visit-thing
  (kbd "TAB") 'magit-section-toggle
  "]]" 'magit-section-forward "[[" 'magit-section-backward
  "J" 'magit-section-forward-sibling "K" 'magit-section-backward-sibling
  "^" 'magit-section-up
  "f" 'magit-fetch "F" 'magit-pull
  "b" 'magit-branch "B" 'magit-bisect
  "l" 'magit-log "\C-l" 'magit-log-refresh
  "x" 'magit-delete-thing
  "gs" 'magit-stash
  "g?" 'magit-dispatch
  "!" 'magit-git-command
  "+" 'magit-diff-more-context "-" 'magit-diff-less-context)

(evil-define-key 'motion 'global
  (kbd "<leader>b") 'switch-to-buffer
  (kbd "<leader>f") 'find-file-rec
  [f9] 'compile-or-recompile

  (kbd "<leader>g") 'magit-status
  (kbd "<leader>G") 'magit-file-dispatch)

;;; Language support
(add-hook 'emacs-lisp-mode-hook (lambda () (setq tab-width 8
                                                 indent-tabs-mode nil)))

(straight-use-package 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(straight-use-package 'haskell-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'nix-mode)
(straight-use-package 'cmake-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'julia-mode)

(setq erlang-electric-commands '(erlang-electric-semicolon
                                 erlang-electric-gt
                                 erlang-electric-newline))
