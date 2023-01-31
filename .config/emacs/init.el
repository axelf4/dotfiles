;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

;; Bootstrap the straight.el package manager
(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)

(setq gc-cons-threshold 16777216
      native-comp-async-report-warnings-errors 'silent
      enable-recursive-minibuffers t
      use-short-answers t
      scroll-conservatively most-positive-fixnum ; Do not center cursor after scrolling
      truncate-partial-width-windows nil ; Always soft-wrap
      window-combination-resize t
      select-enable-clipboard nil ; Do not tie unnamed register "" to system clipboard
      xterm-store-paste-on-kill-ring nil
      make-backup-files nil
      auto-save-no-message t
      auto-save-include-big-deletions t
      kill-buffer-delete-auto-save-files t
      tags-revert-without-query t
      tags-add-tables t
      next-error-recenter t
      xref-auto-jump-to-first-xref t
      vc-handled-backends nil ; Disable VC
      comment-multi-line t
      sentence-end-double-space nil ; Single space between sentences
      calendar-week-start-day 1 ; Monday as first day of the week
      ;; Tailor dynamic abbrevs for non-text modes by default
      dabbrev-upcase-means-case-search t
      dabbrev-case-replace nil
      dabbrev-case-distinction nil)
(setq-default tab-width 4)
(custom-set-variables
 '(show-paren-delay 0)) ; Highlight matching parentheses immediately
(menu-bar-mode 0) ; Disable the menu bar
(global-auto-revert-mode)
(delete-selection-mode)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(global-set-key [escape] 'keyboard-escape-quit)

(electric-pair-mode) ; Autopairing
;; Inhibit autopairing in minibuffers
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local electric-pair-inhibit-predicate #'always)))

(defvar-local electric-indent-words ()
  "Words that should cause automatic reindentation.")
(add-hook
 'electric-indent-functions
 (lambda (_ch)
   (save-excursion
     (backward-word)
     (looking-at-p (regexp-opt electric-indent-words)))))

;;; vi emulation
(straight-use-package 'evil)
(straight-use-package 'undo-tree)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-visualstar)
(setq
 ;; Behave more like Vim
 evil-want-C-u-scroll t
 evil-want-C-u-delete t
 evil-want-C-g-bindings t
 evil-start-of-line t
 evil-search-module 'evil-search
 evil-ex-search-highlight-all nil ; No hlsearch
 evil-ex-substitute-case 'sensitive
 evil-toggle-key "" ; Do not map CTRL-Z
 glc-default-span 1 ; Consider only immediately adjacent changes as the same

 evil-undo-system 'undo-tree
 undo-tree-auto-save-history nil
 undo-tree-enable-undo-in-region t
 evil-ex-complete-emacs-commands t
 evil-ex-visual-char-range t ; Evil has characterwise ranges
 evil-want-Y-yank-to-eol t ; Make Y consistent with other capitals
 evil-symbol-word-search t
 evil-split-window-below t evil-vsplit-window-right t
 evil-motion-state-modes ()
 evil-mode-line-format nil
 ;; Default fails to mimic Vim by not wrapping before looking in other buffers
 evil-complete-next-func #'dabbrev-expand)
(evil-mode)
(evil-set-leader 'motion (kbd "SPC"))
(add-hook 'evil-local-mode-hook #'undo-tree-mode)
(add-hook 'evil-local-mode-hook #'evil-visualstar-mode)
(evil-define-key 'normal 'global
  "\C-^" 'evil-switch-to-windows-last-buffer
  "\C-a" 'evil-numbers/inc-at-pt "\C-x" 'evil-numbers/dec-at-pt
  (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental
  (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental
  "U" 'undo-tree-visualize)
(evil-define-key 'visual 'global "u" nil)
(evil-define-key 'insert 'global
  "\C-f" 'indent-according-to-mode
  ;; Continue comment on new line
  [remap newline]
  '(menu-item "" default-indent-new-line :filter
              (lambda (_cmd)
                (when (save-excursion (comment-beginning))
                  `(lambda () (interactive) (,comment-line-break-function))))))
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
(define-key evil-normal-state-map [remap evil-goto-last-change]
  (evil-define-motion nil (count)
    "Like `goto-last-change' but go to the penultimate change if already there."
    (let ((old-pos (point)))
      (evil-goto-last-change count)
      (when (and (null count) (<= (abs (- old-pos (point))) glc-default-span))
        (setq last-command this-command)
        (goto-last-change nil)))))
;; Move only vertically with gj/gk despite tracking EOL
(defun reset-curswant (&rest _)
  "Unstick the cursor from the end of the line."
  (when (eq temporary-goal-column most-positive-fixnum)
    (setq temporary-goal-column 0)))
(advice-add #'evil-next-visual-line :before #'reset-curswant)
(advice-add #'evil-previous-visual-line :before #'reset-curswant)
;; Inherit split window's previous buffers
(let ((f (lambda (fun &rest args)
           (let ((prev-buffers (copy-sequence (window-prev-buffers))))
             (apply fun args)
             (set-window-prev-buffers nil prev-buffers)))))
  (advice-add #'evil-window-split :around f)
  (advice-add #'evil-window-vsplit :around f))

(defun comment-join-line (beg end)
  "Join lines in the BEG .. END region with comment leaders removed."
  (comment-normalize-vars t)
  (let ((prefix (when fill-prefix (regexp-quote fill-prefix)))
        (erei (comment-padleft
               (comment-string-reverse (or comment-continue comment-start)) 're))
        spt next-linec-pt)
    (save-restriction
      (narrow-to-region
       (progn (goto-char beg) (or (comment-beginning) beg))
       (progn (goto-char (max (1- end) beg))
              (end-of-line (when (<= (line-beginning-position) beg) 2)) (point)))
      (insert ?\n)
      (goto-char (point-min))
      (while (setq spt (comment-search-forward (point-max) t))
        (let ((npt (line-beginning-position 2)) (iept (point-max)))
          (if (when (progn (goto-char spt) (comment-forward))
                (setq iept (save-excursion (comment-enter-backward) (point)))
                (= (point) npt)) ; Line comments generally include NL
              (progn ; Join line comments
                (when (eql spt next-linec-pt) (uncomment-region spt (point)))
                (setq next-linec-pt (progn (skip-chars-forward " \t") (point))))
            (save-excursion ; Eliminate continuation markers
              (goto-char (max beg spt))
              (while (progn (forward-line) (< (point) iept))
                (when (looking-at erei)
                  (setq iept (- iept (- (match-end 0) (match-beginning 0))))
                  (replace-match "" t t)))))))
      (goto-char beg)
      (setq end nil)
      (while (progn (forward-line) (not (eobp)))
        (delete-region
         (setq end (1- (point)))
         (progn (and prefix (looking-at prefix) (goto-char (match-end 0)))
                (skip-chars-forward " \t") (point)))
        (or (memq (following-char) '(0 ?\n ?\)))
            (memq (preceding-char) '(0 ?\n ?\t ?\s))
            (insert ?\s))))
    (delete-char -1)
    (goto-char (or end (signal 'end-of-buffer nil)))))
(advice-add #'evil-join :override #'comment-join-line)

(evil-define-operator evil-comment (beg end)
  "Toggle comment from BEG to END."
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

;; Reinitialize the preexistent "*Messages*" buffer
(with-current-buffer (messages-buffer) (evil-normalize-keymaps))

(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'xref-goto-xref)

;; System clipboard support while running in terminal
(straight-use-package 'xclip)
(ignore-error file-error ; Silence missing backend error
  (xclip-mode))

;;; Minibuffer completion
(straight-use-package 'vertico)
(straight-use-package 'hotfuzz)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-styles '(hotfuzz)
      completion-category-defaults nil
      completion-in-region-function #'minibuffer-completion-in-region)
(vertico-mode)
(hotfuzz-vertico-mode)

(defun minibuffer-completion-in-region (start end collection &optional predicate)
  "Read from minibuffer to complete text between START and END using COLLECTION."
  (if-let ((completion
            (completing-read "Completion: " collection predicate nil
                             (buffer-substring-no-properties start end))))
      (let ((minibuffer-completion-table collection)
            (minibuffer-completion-predicate predicate))
        (completion--replace
         start end (setq completion (copy-sequence completion)))
        (completion--done completion 'unknown)
        t)
    (unless completion-fail-discreetly (completion--message "No completions"))
    nil))

;;; Project management
(set-frame-parameter nil 'cwd default-directory) ; For the initial frame
(push 'cwd frame-inherited-parameters)
(defun cwd ()
  "Get the frame-local current working directory."
  (frame-parameter nil 'cwd))
(defun with-cwd (fun &rest args)
  "Call FUN with ARGS and the current working directory as the default directory."
  (let ((default-directory (cwd))) (apply fun args)))
(add-hook
 'server-after-make-frame-hook
 (lambda ()
   (when-let ((client (frame-parameter nil 'client)))
     (set-frame-parameter nil 'cwd (process-get client 'server-client-directory)))))
(advice-add #'evil-ex :around #'with-cwd)

(let ((find-files-program
       (cond ((executable-find "rg") '("rg" "--color=never" "--files"))
             ((executable-find "find") '("find" "-type" "f")))))
  (defun find-file-rec ()
    "Find a file in the current working directory recursively."
    (interactive)
    (find-file
     (completing-read "Find file: "
                      (apply #'process-lines find-files-program)))))
(advice-add #'find-file-rec :around #'with-cwd)

;; File browsing
(setq dired-auto-revert-buffer #'dired-directory-changed-p
      dired-dwim-target t
      dired-listing-switches "-Ahl --group-directories-first")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(evil-set-initial-state 'wdired-mode 'normal)
(evil-define-key nil dired-mode-map (kbd "SPC") nil)
(evil-define-key 'normal dired-mode-map
  "j" 'dired-next-line "k" 'dired-previous-line
  "e" 'find-file "I" 'dired-toggle-read-only)
(evil-define-key nil wdired-mode-map
  [remap evil-write] 'wdired-finish-edit)

(defun sudo-file-name (file)
  "Return TRAMP file name for editing FILE as root with sudo."
  (let ((remote-id (copy-sequence (file-remote-p file))))
    (concat
     (if remote-id
         (progn (aset remote-id (1- (length remote-id)) ?|) ; Replace :
                remote-id)
       "/")
     "sudo::" (or (file-remote-p file 'localname) file))))

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
(advice-add #'ffap-read-file-or-url :override
            (lambda (_prompt guess) (or guess (user-error "Can't find file"))))

;;; Customize mode line
(setq-default
 mode-line-buffer-identification
 '(:eval
   (if buffer-file-name
       (let* ((dir (abbreviate-file-name
                    (string-remove-prefix (expand-file-name (cwd))
                                          (file-name-directory buffer-file-name))))
              (parts (split-string dir "/")))
         (list (if (length< parts 5) dir
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

;;; Reading documentation
(setq help-window-select t
      describe-bindings-outline t)
(evil-define-key 'normal help-mode-map
  "\C-t" 'help-go-back
  "s" 'help-view-source)

(straight-use-package 'devdocs)
(setq devdocs-window-select t)
(add-hook 'devdocs-mode-hook (lambda () (kill-local-variable 'truncate-lines)))
(define-key help-map "D" 'devdocs-lookup)

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
(advice-add #'compile-or-recompile :around #'with-cwd)

;;; Colorscheme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox t)

;;; Magit
(straight-use-package 'magit)
(setq git-commit-summary-max-length 50
      git-commit-fill-column 72
      magit-status-goto-file-position t)
(with-eval-after-load 'transient
  (define-key transient-base-map [escape] 'transient-quit-one)
  (define-key transient-sticky-map [escape] 'transient-quit-seq))
(dolist (mode '(git-rebase-mode
                magit-status-mode
                magit-log-mode
                magit-log-select-mode
                magit-revision-mode
                magit-diff-mode
                magit-process-mode
                magit-stash-mode
                magit-stashes-mode))
  (evil-set-initial-state mode 'normal))
(with-eval-after-load 'git-rebase
  ;; Edit rebase sequences as ordinary text
  (add-hook 'git-rebase-mode-hook (lambda () (setq buffer-read-only nil)))
  (set-keymap-parent (setq git-rebase-mode-map (make-sparse-keymap))
                     global-map))
(evil-define-key 'normal magit-section-mode-map
  (kbd "TAB") 'magit-section-toggle "^" 'magit-section-up
  "[[" 'magit-section-backward "]]" 'magit-section-forward
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

(with-eval-after-load 'smerge-mode
  (require 'transient)
  (transient-define-prefix smerge-dispatch ()
    "Invoke an SMerge command from a list of available commands."
    [["Keep"
      ("b" "Base" smerge-keep-base)
      ("u" "Upper" smerge-keep-upper)
      ("l" "Lower" smerge-keep-lower)
      ("a" "All" smerge-keep-all) ("RET" "Current" smerge-keep-current)]
     ["Diff"
      ("<" "Base/upper" smerge-diff-base-upper)
      ("=" "Upper/lower" smerge-diff-upper-lower)
      (">" "Base/lower" smerge-diff-base-lower)
      ("R" "Refine" smerge-refine :transient t)]
     ["Other"
      ("C" "Combine" smerge-combine-with-next)
      ("r" "Resolve" smerge-resolve) ("x" "Kill current" smerge-kill-current)]])
  (define-key (plist-get smerge-text-properties 'keymap)
    (kbd "RET") '(menu-item "" smerge-dispatch :enable (evil-normal-state-p))))
(evil-define-motion evil-forward-conflict (count)
  "Move the cursor to the beginning of the COUNT-th next conflict."
  :jump t
  (require 'smerge-mode)
  (smerge-next count)
  (unless smerge-mode (smerge-mode)))
(evil-define-motion evil-backward-conflict (count)
  "Move the cursor to the beginning of the COUNT-th previous conflict."
  :jump t :type inclusive
  (require 'smerge-mode)
  (smerge-prev count)
  (unless smerge-mode (smerge-mode)))

;;; Snippets
(straight-use-package 'yasnippet)
(yas-global-mode)
(add-hook
 'yas-keymap-disable-hook
 (lambda ()
   (when-let* (((not (evil-normal-state-p)))
               (snippet (car-safe (yas-active-snippets)))
               (active-field (yas--snippet-active-field snippet)))
     (/= (yas--field-start active-field) (point)))))

;;; Insert mode completion
(straight-use-package 'company)
(with-eval-after-load 'company
  (setq company-idle-delay nil
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '((company-yasnippet company-capf :separate)))
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (evil-define-key nil company-active-map
    [escape] 'company-abort
    "\C-w" nil ; Do not shadow `evil-delete-backward-word'
    (kbd "TAB") 'company-complete-common-or-cycle
    [backtab] 'company-select-previous))
(define-key global-map [remap indent-for-tab-command]
  (lambda (arg)
    "Perform symbol completion and/or indent the line if in the left margin.
Differs from having `tab-always-indent' set to `complete' in that it
always tries to complete if point is right of the left margin. This
facilitates completion even in programming language modes that do TAB
cycle indentation where you otherwise would only be cycling forever."
    (interactive "P")
    (setq this-command 'indent-for-tab-command)
    (unless (bound-and-true-p company-mode) (company-mode))
    (if (> (current-column) (current-indentation))
        (company-complete-common)
      (company-indent-or-complete-common arg))))

(defun my-company-files (command &rest r)
  "`company-mode' completion backend for existing file names.
Unlike `company-files' relative paths do not have to begin with \"./\",
and the value of `completion-styles' is used."
  (interactive (list 'interactive))
  (let ((f (lambda ()
             (require 'ffap)
             (ffap-string-at-point)
             `(,@ffap-string-at-point-region
               completion-file-name-table
               :predicate ,(lambda (x) (not (string= x "./")))
               :company-kind
               ,(lambda (x) (if (eq (aref x (1- (length x))) ?/) 'folder 'file))
               :exit-function ; Continue completing descendants of directory
               ,(lambda (string _status)
                  ;; company-backend is let-bound here, but not during company-after-completion-hook
                  (cl-labels ((f (_)
                                 (remove-hook 'company-after-completion-hook #'f)
                                 (company-begin-backend 'my-company-files)))
                    (when (eq (aref string (1- (length string))) ?/)
                      (add-hook 'company-after-completion-hook #'f))))))))
    (if (eq command 'interactive)
        (progn (unless (bound-and-true-p company-mode) (company-mode))
               (company-begin-backend 'my-company-files))
      (let ((completion-at-point-functions (list f)))
        (apply #'company-capf command r)))))

;;; Language server protocol
(straight-use-package 'lsp-mode)
(setq lsp-keymap-prefix "<leader> l"
      lsp-enable-symbol-highlighting nil
      lsp-enable-text-document-color nil
      lsp-enable-folding nil
      lsp-auto-execute-action nil
      lsp-enable-suggest-server-download nil
      lsp-completion-default-behaviour :insert
      lsp-rust-analyzer-server-display-inlay-hints t)
(with-eval-after-load 'lsp-mode
  (add-hook
   'prog-mode-hook
   (lambda ()
     "Start lsp mode if the current buffer is part of a session."
     (and buffer-file-name
          (lsp-find-session-folder (lsp-session) buffer-file-name)
          (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                       #'lsp--server-binary-present?))
          (lsp)))))

;;; Spell checking
(setq ispell-silently-savep t)
(advice-add #'evil-next-flyspell-error :before
            (lambda (&rest _) (unless (bound-and-true-p flyspell-mode)
                                (flyspell-mode) (flyspell-buffer))))
(with-eval-after-load 'flyspell
  (define-key flyspell-mouse-map
    (kbd "RET") '(menu-item "" ispell-word :enable (evil-normal-state-p))))

(evil-define-key 'normal 'global
  "gc" 'evil-comment
  (kbd "<leader>u") 'universal-argument
  (kbd "<leader>h") 'help-command
  (kbd "<leader>b") 'switch-to-buffer
  (kbd "<leader>f") 'find-file-rec
  (kbd "<leader>F") 'dired-jump
  [f9] 'compile-or-recompile
  (kbd "<leader>e") 'pp-eval-last-sexp
  (kbd "<leader>E") 'eval-defun

  (kbd "<leader>g")
  (lambda ()
    "Run `magit-status' in the root of the current project."
    (interactive)
    (magit-status-setup-buffer (cwd)))
  (kbd "<leader>G") 'magit-file-dispatch)
(evil-define-key 'motion 'global
  "[c" 'evil-backward-conflict "]c" 'evil-forward-conflict)
(evil-define-key 'insert 'global
  (kbd "C-x C-f") 'my-company-files)

;;; Language support
(add-hook 'emacs-lisp-mode-hook (lambda () (setq tab-width 8
                                                 indent-tabs-mode nil)))

;; CC Mode
(with-eval-after-load 'cc-styles
  (defun c-lineup-conditional-test-clause (_langelem)
    "Indent by `c-basic-offset' times 2 instead of aligning with offset anchor.
Tweaks for loop expressions which otherwise have alignment hardcoded
\(case 7D in `c-guess-basic-syntax'), e.g.:

for (int i;
        i < 3; ++i)
<--><--> c-basic-offset

Works with: statement, statement-cont."
    (let (indent)
      (when (save-excursion (> (c-langelem-col c-syntactic-element)
                               (setq indent (current-indentation))))
        (vector (+ indent (* 2 c-basic-offset))))))
  (c-add-style
   "my-style" ; C/C++ style that uses alignment less liberally
   `((c-comment-only-line-offset 0 . 0) ; Indent column-zero comment lines too
     (c-offsets-alist
      (substatement-open . 0)
      (block-close . c-lineup-under-anchor)
      (statement c-lineup-conditional-test-clause 0)
      (statement-cont add c-lineup-conditional-test-clause +)
      (arglist-cont-nonempty . +)
      (arglist-close
       . ,(lambda (langelem) (if (eq (c-lineup-close-paren langelem) 0) 0 '+)))
      (label . 0) (substatement-label . 0)))))
(setq c-cleanup-list '(comment-close-slash)
      c-electric-pound-behavior '(alignleft)
      c-indent-comments-syntactically-p t
      c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "my-style")))

(straight-use-package 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(add-hook 'sh-mode-hook
          (lambda () (setq electric-indent-words '("fi" "else" "done" "esac"))))

(setq markdown-indent-on-enter 'indent-and-new-item)

(straight-use-package 'typescript-mode)
(defun add-node-modules-to-path ()
  "Add node_modules/.bin to buffer-local `exec-path', if applicable."
  (when-let ((root (locate-dominating-file ; "npm bin" is too slow
                    (or buffer-file-name default-directory)
                    "node_modules")))
    (make-local-variable 'exec-path)
    (cl-pushnew (expand-file-name "node_modules/.bin" root) exec-path
                :test #'string=)))
(add-hook 'typescript-mode-hook #'add-node-modules-to-path)

(straight-use-package 'haskell-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'cmake-mode)
(straight-use-package 'julia-mode)

(straight-use-package 'nix-mode)
(add-hook 'nix-mode-hook
          (lambda () (setq electric-indent-words '("else"))))

(straight-use-package 'lua-mode)
(add-hook 'lua-mode-hook
          (lambda () (setq electric-indent-words '("end" "else"))))

(setq erlang-electric-commands '(erlang-electric-semicolon
                                 erlang-electric-gt
                                 erlang-electric-newline))
(add-hook 'erlang-mode-hook
          (lambda () (setq electric-indent-words '("end"))))

;; Lazily lookup path to Agda mode
(push '("\\.l?agda\\'" .
        (lambda ()
          (load-file (let ((coding-system-for-read 'utf-8))
                       (with-output-to-string
                         (call-process "agda-mode" nil standard-output nil "locate"))))
          (agda2-mode)))
      auto-mode-alist)
