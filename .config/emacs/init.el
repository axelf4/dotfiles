;;; init.el --- Emacs configuration  -*- lexical-binding: t -*-

;; Bootstrap the straight.el package manager
(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)

(setq gc-cons-threshold #x1000000
      native-comp-async-report-warnings-errors 'silent
      enable-recursive-minibuffers t
      translate-upper-case-key-bindings nil
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
      xref-auto-jump-to-first-xref t
      undo-auto-current-boundary-timer t ; Disable automatic undo boundaries
      show-paren-predicate t ; Enable Show Paren Mode in special buffers too
      vc-handled-backends () ; Disable VC
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
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-local-mode 0)))

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

 evil-undo-system 'undo-tree
 undo-tree-auto-save-history nil
 undo-tree-enable-undo-in-region t
 evil-ex-complete-emacs-commands t
 evil-ex-visual-char-range t ; Evil has characterwise ranges
 evil-want-Y-yank-to-eol t ; Make Y consistent with other capitals
 evil-symbol-word-search t
 evil-split-window-below t evil-vsplit-window-right t
 evil-mode-line-format nil
 evil-insert-state-modes '(comint-mode erc-mode)
 evil-motion-state-modes ()
 evil-emacs-state-modes '(debugger-mode))
(evil-mode)
(evil-set-leader 'motion (kbd "SPC"))
(add-hook 'evil-local-mode-hook #'undo-tree-mode)

;; Inherit command-line mappings in minibuffers
(set-keymap-parent minibuffer-local-map evil-command-line-map)
(evil-define-key* nil minibuffer-local-map ; but undo remappings...
  "\C-n" 'next-line "\C-p" 'previous-line)
;; Always use blank lines as paragraph delimiters in motions/text objects
(define-advice forward-evil-paragraph (:around (orig-fun &rest args))
  (let ((paragraph-start (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate))
        (paragraph-ignore-fill-prefix t))
    (apply orig-fun args)))
(define-key
 evil-normal-state-map [remap evil-goto-last-change]
 (evil-define-motion nil (count)
   "Like `evil-goto-last-change' but go to the penultimate change if already there."
   (let ((old-pos (point)))
     (evil-goto-last-change (or count 1))
     (when (or (evil-normal-state-p) (evil-motion-state-p)) (evil-adjust-cursor))
     (and (null count) (= (point) old-pos) (evil-goto-last-change 1)))))
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
(evil-define-key* 'normal special-mode-map [escape] 'quit-window)
;; Visual "*": Search for the selected text instead of the word at point
(define-advice evil-ex-start-word-search
    (:around (oldfun unbounded direction count &optional symbol))
  (if (or (not (evil-visual-state-p)) unbounded)
      (funcall oldfun unbounded direction count symbol)
    (setq deactivate-mark t)
    (cl-letf (((symbol-function #'evil-find-thing)
               (lambda (&rest _)
                 (buffer-substring-no-properties
                  (goto-char evil-visual-beginning) evil-visual-end))))
      (funcall oldfun t direction count))))

(defun format-binary (num)
  "Format non-negative integer NUM in binary."
  (let (digits)
    (while (> num 0)
      (push (+ ?0 (logand num 1)) digits)
      (setq num (ash num -1)))
    (apply #'string (or digits '(?0)))))
(defun search-forward-inc (amount &optional bound)
  "Increment the next number after point ending before BOUND by AMOUNT.
If fail return nil, otherwise set point to the end of the number found
and return non-nil."
  (let (case-fold-search subexp s base)
    (when (re-search-forward
           "0[bB]\\([01]+\\)\\|0\\([0-7]+\\)\\|0[xX]\\([[:xdigit:]]+\\)\\|-?[0-9]+"
           bound t)
      (cond
       ((setq s (match-string (setq subexp 1))) (setq base 2)) ; Binary
       ((setq s (match-string (setq subexp 2))) (setq base 8)) ; Octal
       ((setq s (match-string (setq subexp 3))) (setq base 16)) ; Hexadecimal
       (t (setq s (match-string (setq subexp 0)) base 10))) ; Decimal
      (let ((num (+ (string-to-number s base) amount))
            (fixedcase (not (and case-replace (eq base 16)))))
        (replace-match
         (cond ((eq base 2) (format-binary num))
               ((eq base 8) (format "%o" num))
               ((eq base 10) (number-to-string num))
               ((eq base 16) (format "%x" num)))
         fixedcase fixedcase nil subexp))
      t)))

(defun inc-at-point (count &optional cumulative)
  "Add COUNT to the number at or after the cursor.
If the region is active, do this on every highlighted line. With
non-nil argument CUMULATIVE each line will be incremented by an
additional COUNT."
  (interactive "p")
  (setq deactivate-mark t)
  (cond
   ((not (evil-visual-state-p))
    ;; Move backward to start of number
    (if (when (<= ?0 (char-after) ?1)
          (skip-chars-backward "01")
          (and (eq (char-before (1- (point))) ?0) (eq (upcase (char-before)) ?B)))
        (backward-char 2)
      (let ((old-pos (point)))
        (if (when-let ((ch (char-after)) ((or (<= ?0 ch ?9) (<= ?A (upcase ch) ?F))))
              (skip-chars-backward "[:xdigit:]")
              (and (eq (char-before (1- (point))) ?0) (eq (upcase (char-before)) ?X)))
            (backward-char 2)
          (goto-char old-pos)
          (when (<= ?0 (char-after) ?9)
            (skip-chars-backward "0-9")
            (when (eq (char-before) ?-) (backward-char))))))
    (unless (search-forward-inc count (line-end-position))
      (user-error "No number here"))
    (backward-char))
   ((eq (evil-visual-type) 'block)
    (evil-apply-on-block
     (let ((amount count))
       (lambda (beg end) (goto-char beg)
         (and (search-forward-inc amount end)
              cumulative (setq amount (+ amount count)))))
     (goto-char evil-visual-beginning) evil-visual-end nil))
   (t (goto-char evil-visual-beginning)
      (let ((amount count))
        (while (and (< (point) evil-visual-end)
                    (search-forward-inc amount evil-visual-end))
          (when cumulative (setq amount (+ amount count)))
          (forward-line)))
      (goto-char evil-visual-beginning))))
(defun dec-at-point (count)
  (interactive "p")
  (inc-at-point (- count)))
(defun inc-at-point-cumulative (count)
  (interactive "p")
  (inc-at-point count t))
(defun dec-at-point-cumulative (count)
  (interactive "p")
  (inc-at-point (- count) t))

(defvar-local goto-chg--state nil
  "Change list iterator for \\[evil-goto-last-change].
Either nil or a tuple \(CACHE ALL ITER . DELTAS) where CACHE is a
zipper \(LATER-POSS . EARLIER-POSS) of change positions already moved
to, ALL is the sorted concatenation of LATER-POSS and EARLIER-POSS,
ITER is the remaining tail of `buffer-undo-list', or next
`buffer-undo-tree' node if undo-tree is enabled, and DELTAS is seen
change digests in chronological order.")

(defun goto-chg--reset (&rest _)
  (remove-hook 'after-change-functions #'goto-chg--reset t)
  (setq goto-chg--state nil))

(evil-define-motion older-change (count)
  "Go to COUNT older position in change list.
Start over if changes have been made. If two changes are on the same
line less than `fill-column' or 79 columns apart only the last one is
considered."
  (interactive "p")
  (cl-destructuring-bind (cache all l . deltas)
      (or goto-chg--state
          (progn (add-hook 'after-change-functions #'goto-chg--reset nil t)
                 (list (cons () ()) ()
                       (if (not (bound-and-true-p undo-tree-mode)) buffer-undo-list
                         (undo-list-transfer-to-tree)
                         (undo-tree-current buffer-undo-tree)))))
    (when-let ((p (car (if (< count 0) (car cache) (cdr cache))))
               ((= (if (not (or (evil-normal-state-p) (evil-motion-state-p))) p
                     (save-excursion (goto-char p) (evil-adjust-cursor) (point)))
                   (point))))
      (setq count (+ count (cl-signum count))))
    (while (and (< count 0) (car cache))
      (setq count (1+ count)) (push (goto-char (pop (car cache))) (cdr cache)))
    (while (and (> count 0) (cdr cache))
      (setq count (1- count)) (push (goto-char (pop (cdr cache))) (car cache)))
    (while (and (> count 0) l)
      (let ((list (if (bound-and-true-p undo-tree-mode) (undo-tree-node-undo l) l))
            (old-deltas deltas) pos)
        ;; Collect deltas and position of most recent change in this changeset
        (while (pcase (pop list)
                 ('nil nil) ; Undo boundary
                 (`(,(and (pred integerp) beg) . ,end) ; Insertion
                  (unless pos (setq pos (1- end)))
                  (push (cons beg (cons beg (- end beg))) deltas))
                 (`(,(and (pred stringp) text) . ,position) ; Deletion
                  (let ((beg (abs position)) (len (length text)))
                    (unless pos (setq pos beg))
                    (push (cons beg (cons (+ beg len) (- len))) deltas)))
                 (`(nil ,_property ,_value ,_beg . ,end) ; Textprop change
                  (if pos t (setq pos end)))
                 (`(apply ,delta ,beg ,end ,_fun-name . ,_args)
                  (unless pos (setq pos end))
                  (push (cons beg (cons end delta)) deltas))
                 (_ t)))
        (when pos
          ;; Adjust position by applying future deltas
          (cl-loop for (beg end . delta) in old-deltas when (< beg pos)
                   do (setq pos (+ (if (< pos end) end pos) delta)))
          (cl-destructuring-bind (&whole tail &optional left right . _)
              (cl-loop for xs on all and prev = nil then xs while (< (car xs) pos)
                       finally return (or prev (cons nil xs)))
            (cl-flet ((too-close-p (min max)
                        (when (< (abs (- min max)) (if auto-fill-function fill-column 79))
                          (save-excursion (goto-char min) (not (search-forward "\n" max t))))))
              (unless (or (and left (too-close-p left pos))
                          (and right (too-close-p pos right)))
                (setq count (1- count))
                (push pos (if left (cdr tail) all))
                (push (goto-char pos) (car cache))))))
        (setq l (if (bound-and-true-p undo-tree-mode) (undo-tree-node-previous l) list))))
    (setq goto-chg--state (cons cache (cons all (cons l deltas)))))
  (or (= count 0) (error "No %s change info" (if (< count 0) "later" "further"))))
(defalias #'evil-goto-last-change #'older-change)

(evil-define-motion newer-change (count)
  "Go to COUNT newer position in change list.
Just like \\[evil-goto-last-change] but in the opposite direction."
  (interactive "p")
  (older-change (- count)))
(defalias #'evil-goto-last-change-reverse #'newer-change)

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
                (= (point) npt)) ; Line comments include NL
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
      read-extended-command-predicate #'command-completion-default-include-p
      completion-styles '(hotfuzz)
      completion-category-defaults ()
      completion-category-overrides `((eglot (styles . ,completion-styles))))
(vertico-mode)
(hotfuzz-vertico-mode)

(defun minibuffer-completion-in-region (start end collection &optional predicate)
  "Read from minibuffer to complete text between START and END using COLLECTION."
  (if-let (completion (completing-read "Completion: " collection predicate nil
                                       (buffer-substring-no-properties start end)))
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
    (find-file (completing-read
                "Find file: " (apply #'process-lines find-files-program)))))
(advice-add #'find-file-rec :around #'with-cwd)

;; File browsing
(setq dired-auto-revert-buffer #'dired-directory-changed-p
      dired-dwim-target t
      dired-recursive-copies 'always dired-recursive-deletes 'always
      dired-listing-switches "-Ahl --group-directories-first")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(evil-define-key nil dired-mode-map
  (kbd "SPC") nil
  [remap evil-next-line] 'dired-next-line [remap evil-previous-line] 'dired-previous-line)
(evil-define-key 'normal dired-mode-map
  "f" 'find-file "I" 'dired-toggle-read-only)
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
(define-advice ffap-read-file-or-url (:override (_prompt guess))
  (or guess (user-error "Can't find file")))

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
   (:eval (let ((len (string-width (format-mode-line mode-line-position))))
            (propertize " " 'display `(space :align-to (- right ,(1+ len))))))
   mode-line-position))

;;; Reading documentation
(setq help-window-select t
      help-window-keep-selected t)
(evil-define-key* 'normal help-mode-map
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
      compilation-buffer-name-function ; Make compilations unique per frame
      (lambda (name-of-mode)
        (concat "*" (downcase name-of-mode) "-" (frame-parameter nil 'name) "*"))
      compilation-environment '("TERM=xterm-256color"))
;; Interpret ANSI escape sequences in compilation output
(advice-add #'compilation-filter :filter-args
            (cl-function (lambda ((proc string))
                           (list proc (xterm-color-filter string)))))
(define-advice compilation-start (:around (fun &rest args))
  (let ((compilation-environment (append compilation-environment
                                         (frame-parameter nil 'environment))))
    (apply fun args)))
(defun compile-or-recompile ()
  "Redo a previous compilation if such exists or prompt for a command.
Unlike `recompile' it is not necessary to run this in the Compilation
mode buffer."
  (interactive)
  (require 'compile)
  (if-let ((buf (get-buffer (compilation-buffer-name
                             "compilation" 'compilation-mode nil))))
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
      magit-define-global-key-bindings nil
      magit-status-goto-file-position t
      magit-diff-refine-hunk t)
(with-eval-after-load 'transient
  (define-key transient-base-map [escape] 'transient-quit-one)
  (define-key transient-sticky-map [escape] 'transient-quit-seq))
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
  "gz" 'magit-stash
  "gr" 'magit-refresh "gR" 'magit-refresh-all
  "g?" 'magit-dispatch "!" 'magit-git-command
  "+" 'magit-diff-more-context "-" 'magit-diff-less-context)

(with-eval-after-load 'smerge-mode
  (define-key
   (plist-get smerge-text-properties 'keymap) (kbd "RET")
   `(menu-item "" ,smerge-basic-map :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd))))
  (define-key smerge-basic-map (kbd "RET") 'smerge-keep-current))
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

(defun yas-completion-at-point (&optional interactive)
  "Complete YASnippet snippets."
  (interactive "p")
  (if interactive
      (let ((completion-at-point-functions '(yas-completion-at-point)))
        (completion-at-point))
    (pcase (cl-letf (((symbol-function #'yas--fetch)
                      (lambda (table prefix) ; Filter by prefix instead of exact match
                        (cl-loop
                         with regex = (completion-pcm--pattern->regex prefix)
                         for key being the hash-keys of (yas--table-hash table)
                         using (hash-values namehash) if (string-match-p regex key)
                         nconc (yas--namehash-templates-alist namehash) into xs
                         finally return (yas--filter-templates-by-condition xs)))))
             (yas--templates-for-key-at-point))
      (`(,templates . ,region)
       (let ((table (make-hash-table :test #'equal :size (length templates))))
         (cl-loop for (_name . template) in templates do
                  (puthash (yas--template-key template) template table))
         `(,@region
           ,table
           :company-kind ,(lambda (_) 'snippet)
           :annotation-function ,(lambda (s) (yas--template-name (gethash s table)))
           :exit-function
           ,(lambda (s _status)
              (yas-expand-snippet (gethash s table) (- (point) (length s)) (point)))))))))

;;; Insert mode completion
(straight-use-package 'corfu)
(with-eval-after-load 'corfu
  (let ((kind-text-mapping
         '((variable "v" . font-lock-variable-name-face)
           (file "f" . font-lock-string-face)
           (folder "d" . font-lock-doc-face)
           (function "f" . font-lock-function-name-face)
           (keyword "k" . font-lock-keyword-face)
           (snippet "S" . font-lock-string-face))))
    (defun kind-margin-formatter (_metadata)
      (when-let ((kind-fun (plist-get completion-extra-properties :company-kind)))
        (lambda (s)
          (if-let (x (alist-get (funcall kind-fun s) kind-text-mapping))
              (let ((s (format " %s " (car x)))
                    (face `(:foreground ,(face-attribute (cdr x) :foreground) :weight bold)))
                (put-text-property 0 (length s) 'face face s)
                s)
            "")))))
  (setq corfu-quit-at-boundary t corfu-quit-no-match t
        corfu-cycle t
        corfu-margin-formatters '(kind-margin-formatter))
  (evil-define-key* nil corfu-map
    [escape] 'corfu-quit (kbd "TAB") 'corfu-next [backtab] 'corfu-previous)
  (evil-make-overriding-map corfu-map)
  (dolist (f '(corfu--setup corfu--teardown))
    (advice-add f :after #'evil-normalize-keymaps))

  (defvar-local corfu-terminal--ov nil)
  (cl-defun corfu-terminal--popup-show (pos off width lines &optional curr lo bar &aux save-pos buffer)
    "Show the Corfu `corfu--popup-show' completion popup using overlays."
    (when corfu-terminal--ov (delete-overlay corfu-terminal--ov))
    (setq save-pos (point)
          buffer (make-indirect-buffer (current-buffer) (generate-new-buffer-name " *temp*") nil t))
    (unwind-protect
        (cl-loop
         with (x . y) = (posn-x-y pos)
         with col = (min (max 0 (- x (line-number-display-width) off))
                         (- (window-text-width) width 5))
         and dir = (if (< (+ y (length lines)) (window-text-height)) 1 -1)
         with sp = (save-excursion (vertical-motion (max 0 dir)) (point))
         initially (vertical-motion (cons (if (< dir 0) 0 (- (window-width) 2)) 0))
         for i from 0 and line in lines and op = sp then np with np and nl-p and j = 1 and xs do
         (let ((p0 (point))
               (p1 (progn (setq nl-p (/= (vertical-motion (cons col (* dir j))) 0)) (point)))
               col0 (face (if (= i curr) 'corfu-current 'corfu-default))
               (bar (if (and lo (<= lo i (+ lo bar))) #(" " 0 1 (face corfu-bar))
                      #(" " 0 1 (face corfu-current)))))
           (setq j (if (= p1 p0) (1+ j) 1)
                 np (max p1 (save-excursion (vertical-motion (cons (+ col width 2) 0)) (point))))
           (when (< dir 0) (cl-rotatef p1 np))
           (when (> op p1) (cl-rotatef op p1))
           ;; One visual line can be many logical lines (e.g. fold overlay)
           (with-current-buffer buffer
             (cl-destructuring-bind (pos _hpos vpos . _)
                 (compute-motion op (cons (+ col width 2) 0) p1 (cons col 1)
                                 nil (cons (window-hscroll) 0) nil)
               (push (buffer-substring op (if (> vpos 1) (setq pos (1- pos)) pos)) xs)
               (setq col0 (if (and (not (and (> dir 0) (= i 0) nl-p)) (< vpos 1))
                              (progn (push #(" \n" 0 1 (cursor 1)) xs) 0)
                            (goto-char (if (< dir 0) op pos)) (vertical-motion 0) (current-column)))))
           (let ((l (concat line (propertize " " 'display `(space :align-to (,(+ col0 col width 1)))))))
             (add-face-text-property 0 (length l) face nil l)
             (push (concat (propertize " " 'display `(space :align-to ,(+ col0 col))) l bar) xs)))
         finally (goto-char np)
         (if (> dir 0) (setq xs (nreverse xs))
           (while (and (> j 1) (/= (vertical-motion (cons (- (window-width) 2) (- j))) 0) (= (point) np))
             (setq j (1+ j)) (push "\n" xs)))
         (let ((ov (setq corfu-terminal--ov (make-overlay sp (point) nil t t)))
               (s (apply #'concat xs)))
           (remove-list-of-text-properties 0 (length s) '(line-prefix) s)
           (add-face-text-property 0 (length s) 'default t s)
           (overlay-put ov 'window (selected-window))
           (overlay-put ov 'before-string s)
           (overlay-put ov 'display ""))))
    (kill-buffer buffer) (goto-char save-pos))
  (defun corfu-terminal--popup-hide ()
    "Hide Corfu overlays popup."
    (delete-overlay corfu-terminal--ov))
  (fset #'corfu--popup-support-p #'always)
  (fset #'corfu--popup-show #'corfu-terminal--popup-show)
  (fset #'corfu--popup-hide #'corfu-terminal--popup-hide)
  ;; Restrict size of Corfu popup to fit in window
  (define-advice corfu--candidates-popup (:around (orig-fun pos))
    (let* ((y (cdr (posn-x-y pos)))
           (corfu-max-width (min corfu-max-width (- (window-text-width) 5)))
           (corfu-count (min corfu-count (max y (- (window-text-height) y 1)))))
      (funcall orig-fun pos))))
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if (minibufferp) #'minibuffer-completion-in-region
                 (corfu-mode) completion-in-region-function)
               args)))
(define-key global-map [remap indent-for-tab-command]
  (lambda (arg)
    "Perform symbol completion and/or indent if in the left margin.
Differs from having `tab-always-indent' set to `complete' in that it
always tries to complete if point is right of the left margin.
Otherwise in programming language modes that do TAB cycle indentation
you would only ever cycle."
    (interactive "P")
    (setq this-command 'indent-for-tab-command)
    (if (> (current-column) (current-indentation))
        (completion-at-point)
      (let ((tab-always-indent 'complete) transient-mark-mode)
        (indent-for-tab-command arg)))))
;; Insert completion without overwriting text right of cursor
(define-advice completion--capf-wrapper (:filter-return (res) nil -1)
  (and (consp (cdr-safe res)) (not (functionp (cdr res)))
       (setcar (nthcdr 2 res) (point))) ; Set `end' to point
  res)

(defun file-completion-at-point (&optional interactive)
  "Complete file name at point."
  (interactive "p")
  (if interactive
      (let ((completion-at-point-functions '(file-completion-at-point)))
        (completion-at-point))
    (require 'ffap)
    (ffap-string-at-point)
    `(,@ffap-string-at-point-region completion-file-name-table
      :predicate ,(lambda (s) (not (string= s "./"))) :exclusive no
      :company-kind
      ,(lambda (s) (if (eq (aref s (1- (length s))) ?/) 'folder 'file))
      :exit-function ; Continue completing descendants of directory
      ,(lambda (s _status)
         (when (eq (aref s (1- (length s))) ?/) (file-completion-at-point t))))))

;;; Language server protocol
(setq eglot-extend-to-xref t
      eglot-ignored-server-capabilities '(:documentHighlightProvider))
(advice-add #'eglot--current-project :around #'with-cwd)
(advice-add #'jsonrpc--log-event :override #'ignore)

;;; Spell checking
(setq ispell-silently-savep t)
(define-advice evil-next-flyspell-error (:before (&rest _))
  (unless (bound-and-true-p flyspell-mode) (flyspell-mode) (flyspell-buffer)))
(with-eval-after-load 'flyspell
  (define-key flyspell-mouse-map (kbd "RET")
    `(menu-item "" ispell-word :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd)))))

;;; Email
(setq send-mail-function #'sendmail-send-it ; Use external "sendmail" program
      message-kill-buffer-on-exit t
      message-default-mail-headers "Cc: \nBcc: \n"
      message-directory "~/.mail/"
      notmuch-search-oldest-first nil
      notmuch-search-result-format
      `(("date" . "%-12s ") ("authors" . "%-20s ") ("subject" . "%s ")
        (,(cl-function
           (lambda (_format-string (&key matched total &allow-other-keys))
             (format (cond ((= total 1) "")
                           ((< matched total) #("%d/%d " 0 5 (face notmuch-search-count)))
                           (t #("%2$d " 0 4 (face notmuch-search-count))))
                     matched total))))
        ("tags" . "%s"))
      notmuch-fcc-dirs '(("axel@axelf.se" . "\"gmail/[Gmail]/Sent Mail\" +sent")))
(with-eval-after-load 'notmuch
  (defun notmuch-delete (&optional undelete)
    "Delete each message in the currently selected thread.
If a prefix argument is given, the messages will be \"undeleted\"."
    (interactive "P")
    (funcall (cl-ecase major-mode
               (notmuch-search-mode #'notmuch-search-tag)
               (notmuch-show-mode #'notmuch-show-tag-all))
             (list (concat (if undelete "-" "+") "deleted"))))
  (defun notmuch-mua-subject-check ()
    "Signal an error if the message subject is missing."
    (unless (message-field-value "Subject")
      (message-goto-subject)
      (error "Missing subject")))
  (defun notmuch-show-close-read-messages ()
    "Collapse all non-unread messages."
    (goto-char (point-min))
    (while (let ((props (get-text-property (point) :notmuch-message-properties)))
             (unless (member "unread" (plist-get props :tags))
               (notmuch-show-message-visible props nil))
             (notmuch-show-goto-message-next))))
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-subject-check)
  (add-hook 'notmuch-show-hook #'notmuch-show-close-read-messages)
  (evil-define-key* 'normal notmuch-common-keymap
    [escape] 'notmuch-bury-or-kill-this-buffer "g?" 'notmuch-help
    "s" 'notmuch-search "S" 'notmuch-tree
    "c" 'notmuch-mua-new-mail "x" 'notmuch-delete
    "gr" 'notmuch-refresh-this-buffer "gR" 'notmuch-poll-and-refresh-this-buffer)
  (evil-define-key* 'normal notmuch-search-mode-map
    (kbd "RET") 'notmuch-search-show-thread
    "A" 'notmuch-search-archive-thread
    "-" 'notmuch-search-remove-tag "+" 'notmuch-search-add-tag)
  (evil-define-key* 'normal notmuch-show-mode-map
    (kbd "RET") 'notmuch-show-toggle-message
    "[[" 'notmuch-show-previous-message "]]" 'notmuch-show-next-message
    "." 'notmuch-show-part-map "A" 'notmuch-show-archive-thread-then-exit
    "r" 'notmuch-show-reply "R" 'notmuch-show-reply-sender
    "-" 'notmuch-show-remove-tag "+" 'notmuch-show-add-tag)
  (evil-define-key* 'normal notmuch-tree-mode-map
    (kbd "RET") 'notmuch-tree-show-message))
;; Autoloads for Nixpkgs packages are not loaded without package.el
(autoload 'notmuch-jump-search "notmuch")

(straight-use-package 'rmsbolt) ; Compiler Explorer

(evil-define-key* 'normal 'global
  "\C-^" 'evil-switch-to-windows-last-buffer
  "\C-a" 'inc-at-point "\C-x" 'dec-at-point
  "U" 'undo-tree-visualize
  "gc" 'evil-comment
  [f9] 'compile-or-recompile
  (kbd "<leader>u") 'universal-argument
  (kbd "<leader>h") 'help-command
  (kbd "<leader>b") 'switch-to-buffer
  (kbd "<leader>f") 'find-file-rec (kbd "<leader>F") 'dired-jump
  (kbd "<leader>e") 'pp-eval-last-sexp (kbd "<leader>E") 'eval-defun

  (kbd "<leader>g")
  (lambda ()
    "Run `magit-status' in the root of the current project."
    (interactive)
    (magit-status-setup-buffer (cwd)))
  (kbd "<leader>G") 'magit-file-dispatch
  (kbd "<leader>m") 'notmuch-jump-search)
(define-key universal-argument-map (kbd "<leader>u") 'universal-argument-more)
(evil-define-key* 'motion 'global
  "[c" 'evil-backward-conflict "]c" 'evil-forward-conflict)
(evil-define-key* 'visual 'global
  "u" nil
  "g\C-a" 'inc-at-point-cumulative "g\C-x" 'dec-at-point-cumulative)
(evil-define-key* 'insert 'global
  "\C-f" 'indent-according-to-mode
  "\C-n" 'dabbrev-expand
  ;; Continue comment on new line
  [remap newline]
  `(menu-item "" default-indent-new-line :filter
              ,(lambda (_cmd)
                 (when (save-excursion (comment-beginning))
                   `(lambda () (interactive) (,comment-line-break-function)))))
  "\C-x\C-f" 'file-completion-at-point)

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

(straight-use-package 'markdown-mode)
(setq markdown-indent-on-enter 'indent-and-new-item)

(add-hook 'latex-mode-hook #'reftex-mode)

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
          (lambda () (setq electric-indent-words '("end" "after"))))

;; Lazily lookup path to Agda mode
(push '("\\.l?agda\\'" .
        (lambda ()
          (load-file (let ((coding-system-for-read 'utf-8))
                       (with-output-to-string
                         (call-process "agda-mode" nil standard-output nil "locate"))))
          (agda2-mode)))
      auto-mode-alist)
