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
      vc-handled-backends nil ; Disable VC
      ;; Tailor dynamic abbrevs for non-text modes by default
      dabbrev-upcase-means-case-search t
      dabbrev-case-replace nil
      dabbrev-case-distinction nil)
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
(straight-use-package 'evil-visualstar)
(setq
 ;; Behave more like Vim
 evil-want-C-u-delete t
 evil-want-C-u-scroll t
 evil-start-of-line t
 evil-search-module 'evil-search
 evil-ex-search-highlight-all nil ; No hlsearch
 evil-ex-substitute-case 'sensitive
 evil-toggle-key "" ; Do not map CTRL-Z
 glc-default-span 1 ; Consider only immediately adjacent changes as the same

 evil-undo-system 'undo-tree
 undo-tree-auto-save-history nil
 undo-tree-enable-undo-in-region t
 evil-want-Y-yank-to-eol t ; Make Y consistent with other capitals
 evil-symbol-word-search t
 evil-split-window-below t evil-vsplit-window-right t
 evil-motion-state-modes '()
 ;; Default fails to mimic Vim by not wrapping before looking in other buffers
 evil-complete-next-func #'dabbrev-expand)
(evil-mode)
(evil-set-leader 'motion (kbd "SPC"))
(add-hook 'evil-local-mode-hook #'undo-tree-mode)
(add-hook 'evil-local-mode-hook #'evil-visualstar-mode)
(evil-define-key 'normal 'global
  "\C-a" 'evil-numbers/inc-at-pt "\C-x" 'evil-numbers/dec-at-pt
  (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental
  (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental
  "U" 'undo-tree-visualize)
(evil-define-key 'visual 'global "u" 'evil-undo)
(evil-define-key 'normal special-mode-map [escape] 'quit-window)

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
    "Like `goto-last-change' but go to the penultimate change if already there."
    (interactive "P")
    (setq this-command 'goto-last-change)
    (let ((old-pos (point)))
      (goto-last-change arg)
      (when (<= (abs (- old-pos (point))) glc-default-span)
        (setq last-command this-command)
        (goto-last-change arg)))))
;; Move only vertically with gj/gk despite tracking EOL
(defun reset-curswant (&rest args)
  "Unstick the cursor from the end of the line."
  (when (eq temporary-goal-column most-positive-fixnum)
    (setq temporary-goal-column 0)))
(advice-add #'evil-next-visual-line :before #'reset-curswant)
(advice-add #'evil-previous-visual-line :before #'reset-curswant)

;; Reinitialize the preexistent "*Messages*" buffer
(with-current-buffer (messages-buffer) (evil-normal-state))

(global-set-key (kbd "<leader>h") 'help-command)
(evil-define-key 'normal help-mode-map "\C-t" 'help-go-back)

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
(defun with-project-dir (fun &rest args)
  "Call FUN with ARGS and the project root as the default directory."
  (let ((default-directory (project-root))) (apply fun args)))
(add-hook
 'server-after-make-frame-hook
 (lambda ()
   (when-let ((client (frame-parameter nil 'client)))
     (set-frame-parameter nil 'cwd (process-get client 'server-client-directory)))))
(advice-add #'evil-ex :around #'with-project-dir)

(let ((find-files-program (cond
                           ((executable-find "rg") '("rg" "--color=never" "--files"))
                           ((executable-find "find") '("find" "-type" "f")))))
  (defun find-file-rec ()
    "Find a file in the current working directory recursively."
    (interactive)
    (find-file
     (completing-read "Find file: "
                      (apply #'process-lines find-files-program)))))
(advice-add #'find-file-rec :around #'with-project-dir)

(with-eval-after-load 'grep
  (setq grep-save-buffers nil)
  (when (executable-find "rg")
    ;; Cannot use `grep-apply-setting' since we only want ripgrep
    ;; where we know it is available.
    (grep-compute-defaults) ; Populate defaults
    (setcdr (assq 'localhost grep-host-defaults-alist)
            '((grep-command "rg --no-heading -Hn0 ")
              (grep-use-null-device nil)
              (grep-highlight-matches t)))))
(evil-ex-define-cmd "gr[ep]" #'grep)

;; Suppress confirmation of find-file-at-point guess
(advice-add
 #'ffap-read-file-or-url :override
 (lambda (_prompt guess) (or guess (user-error "Can't find file"))))

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
(evil-declare-not-repeat #'compile-or-recompile)
(advice-add #'compile-or-recompile :around #'with-project-dir)

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
                magit-rebase-mode
                magit-revision-mode
                magit-diff-mode
                magit-process-mode
                magit-stashes-mode))
  (evil-set-initial-state mode 'normal))
(with-eval-after-load 'git-rebase
  ;; Edit rebase sequences as ordinary text
  (add-hook 'git-rebase-mode-hook (lambda () (setq buffer-read-only nil)))
  (set-keymap-parent (setq git-rebase-mode-map (make-sparse-keymap))
                     global-map))
(evil-define-key 'normal magit-section-mode-map
  (kbd "TAB") 'magit-section-toggle "^" 'magit-section-up
  "]]" 'magit-section-forward "[[" 'magit-section-backward
  "J" 'magit-section-forward-sibling "K" 'magit-section-backward-sibling)
(evil-define-key 'normal magit-mode-map
  [remap quit-window] 'magit-mode-bury-buffer
  (kbd "RET") 'magit-visit-thing
  "b" 'magit-branch "B" 'magit-bisect
  "c" 'magit-commit
  "d" 'magit-diff "D" 'magit-diff-refresh
  "f" 'magit-fetch "F" 'magit-pull
  "l" 'magit-log "\C-l" 'magit-log-refresh
  "P" 'magit-push
  "r" 'magit-rebase "R" 'magit-file-rename
  "s" 'magit-stage-file "S" 'magit-stage-modified
  "u" 'magit-unstage "U" 'magit-unstage-all
  "x" 'magit-delete-thing "X" 'magit-reset
  "gs" 'magit-stash
  "gr" 'magit-refresh "gR" 'magit-refresh-all
  "g?" 'magit-dispatch
  "+" 'magit-diff-more-context "-" 'magit-diff-less-context
  "!" 'magit-git-command)

(evil-define-key 'normal 'global
  (kbd "<leader>b") 'switch-to-buffer
  (kbd "<leader>f") 'find-file-rec
  [f9] 'compile-or-recompile

  (kbd "<leader>g")
  (lambda ()
    "Run `magit-status' in the root of the current project."
    (interactive)
    (magit-status-setup-buffer (project-root)))
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
