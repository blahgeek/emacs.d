;;; init --- My config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq
 native-comp-deferred-compilation t
 native-comp-async-jobs-number 8
 native-comp-async-report-warnings-errors nil
 initial-major-mode 'fundamental-mode
 initial-scratch-message ";; This buffer is set to fundamental mode initially to speedup emacs startup. Execute the following line to switch back.\n;; (lisp-interaction-mode)"
 garbage-collection-messages nil)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

(progn  ;; GC tune {{{
  ;; Set to large value before start
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
        gc-cons-percentage 0.6)
  ;; ... and restore it
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 32 (* 1024 1024))
                    gc-cons-percentage 0.1)))
  ;; emacs will never free its heap
  ;; pausing gc will cause huge memory consumption
  ;; (defun my/gc-pause ()
  ;;   "Pause garbage collection for now."
  ;;   (setq gc-cons-threshold (* 2 (* 1024 (* 1024 1024)))))

  ;; (defun my/gc-resume ()
  ;;   "Resume garbage collection (and do it once)."
  ;;   (setq gc-cons-threshold (* 64 (* 1024 1024)))
  ;;   (setq garbage-collection-messages nil)
  ;;   (message "Garbage collecting...done (%.3fs)"
  ;;            (my/timeit (garbage-collect)))
  ;;   (setq garbage-collection-messages t)
  ;;   )

  ;; (add-hook 'focus-out-hook #'my/gc-resume)
  ;; (add-hook 'evil-insert-state-exit-hook #'my/gc-resume)
  ;; (add-hook 'evil-insert-state-entry-hook #'my/gc-pause)
  ;; (add-hook 'minibuffer-setup-hook #'my/gc-pause)
  ;; (add-hook 'minibuffer-exit-hook #'my/gc-resume)
  ) ;;; }}}

(progn  ;; Package Manager: borg {{{
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (setq borg-maketexi-filename-regexp nil)
  (borg-initialize)
  )  ;; }}}

(progn  ;; Package Manager: use-package {{{
  (setq use-package-verbose t
        ;; always defer. this is important
        use-package-always-defer t)

  (eval-when-compile
    (require 'use-package))

  ;; for use-package :delight
  (use-package delight
    :demand t)
  ) ;; }}}

(progn  ;; Profiling, usually disabled {{{
  ;;(use-package keyfreq
  ;;  :config
  ;;  (keyfreq-mode 1)
  ;;  (keyfreq-autosave-mode 1))

  ;;(use-package memory-usage)

  ;; (use-package esup)
  (defun display-startup-echo-area-message ()
    "Override default startup echo message."
    (message (format "Emacs started in %s, welcome" (emacs-init-time))))
  ) ;; }}}

(progn  ;; Some utility helper functions {{{
  (defun my/macos-p ()
    "Return t if it's in macos."
    (string-equal system-type "darwin"))

  (defmacro my/timeit (&rest body)
    "Measure and return the time it takes to evaluate BODY."
    `(let ((time (current-time)))
       ,@body
       (float-time (time-since time))))

  (defmacro comment (&rest body)
    "Comment out one or more s-expressions."
    nil)

  (defmacro my/define-advice (symbol args &rest body)
    "Like `define-advice', but make sure SYMBOL is defined."
    (declare (indent 2) (debug (sexp sexp def-body)))
    (when (length> args 2)
      (setf (nth 2 args) (intern (concat "my/" (symbol-name (nth 2 args))))))
    `(progn
       (unless (symbol-function ',symbol)
         (display-warning 'my/define-advice (format "Function %s is not defined" ',symbol) :error))
       (define-advice ,symbol ,args ,@body)))

  (defvar prog-mode-local-only-hook nil
    "Custom hook for prog-mode, but local only (not triggered for TRAMP file)")
  (defun my/trigger-prog-mode-local-only-hook ()
    "Trigger `prog-mode-local-only-hook' on prog-mode, if it's a local buffer."
    (unless (and (buffer-file-name)
                 (file-remote-p (buffer-file-name)))
      (run-hooks 'prog-mode-local-only-hook)))

  (add-hook 'prog-mode-hook #'my/trigger-prog-mode-local-only-hook)
  )  ;; }}}

(progn  ;; some early settings/hacks
  ;; for some reason, `tags-file-name' would be set as a global variable sometime
  ;; which would make CAPF tags function slow (e.g. emacs .el TAGS are loaded for .cc file)
  (make-variable-buffer-local 'tags-file-name))

(progn  ;; pragmata ligatures and icons {{{
  (use-package ligature
    :config
    (ligature-set-ligatures
     'prog-mode '("!!" "!=" "!!!" "!==" "&&" "***" "*=" "*/" "++" "+=" "--" "-="
                  "->" ".." "..." "/*" "//" "/>" "///" "/**" ":::" "::" ":=" "<-"
                  "<<" "<=" "<=>" "==" "===" "=>" ">=" ">>" "??" "\\\\" "|=" "||"
                  "[[" "]]"))
    :hook (prog-mode . ligature-mode))

  ;; pragmata major mode icons
  (let (delight-args)
    (dolist (pair '((dired-mode . "\xf755")
                    (wdired-mode . (:eval (propertize "\xf756" 'face 'error)))
                    (python-mode . "\xe606")
                    (js-mode . "\xe60c")
                    (sh-mode . "\xe614")
                    (c++-mode . "\xe61d")
                    (c-mode . "\xe61e")
                    (go-mode . "\xe626")
                    (tsx-mode . "\xe796")
                    (lua-mode . "\xe620")
                    (typescript-mode . "\xe628")
                    (vimrc-mode . "\xe62b")
                    (html-mode . "\xe736")
                    (java-mode . "\xe738")
                    (ruby-mode . "\xe739")
                    (markdown-mode . "\xe73e")
                    (haskell-mode . "\xe777")
                    (rust-mode . "\xe7a8")
                    (vterm-mode . "\xe795")
                    (dockerfile-mode . "\xe7b0")))
      (push (list (car pair) (cdr pair) :major) delight-args)
      (let ((ts-mode (intern (string-replace "-mode" "-ts-mode" (symbol-name (car pair))))))
        (when (fboundp ts-mode)
          (push (list ts-mode (concat (cdr pair) "+\xe21c") :major) delight-args))))
    (delight delight-args))

  ;; see delight.el
  (with-eval-after-load 'cc-mode
    (my/define-advice c-update-modeline (:override (&rest _) ignore-for-delight)
      nil))

  ;; https://github.com/fabrizioschiavi/pragmatapro/issues/217
  ;; "…" (\u2026) has a bug in PragmataPro Liga:
  ;;   it's double-char-width in regular weight, but single-char-width in bold weight.
  ;;   (it's usually double-char-width in other fonts)
  ;;
  ;; `truncate-string-ellipsis' returns "…" (\u2026) by default
  ;; and expects it to always be double-char-width (maybe get this info from regular weight?),
  ;; so it would make tables unaligned.
  (setq truncate-string-ellipsis "...")

  ;; Actually we would prefer it to be single-char-width. It looks better in shell git status.
  ;; So we can use the version from PragmataPro Mono Liga.
  (setq use-default-font-for-symbols nil)  ;; this is required to make the next line work
  (set-fontset-font "fontset-default" #x2026 "PragmataPro Mono Liga")

  )  ;; }}}

(progn  ;; EVIL & general keybindings {{{
  (when window-system
    ;; https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
    ;; to define C-m key
    (define-key input-decode-map [?\C-m] [C-m]))

  (use-package evil
    :demand t
    :init
    (setq evil-want-C-w-in-emacs-state t
          evil-want-C-i-jump nil
          evil-split-window-below t
          evil-vsplit-window-right t
          evil-want-fine-undo t
          evil-search-module 'evil-search
          evil-ex-search-vim-style-regexp t
          evil-undo-system 'undo-redo
          evil-symbol-word-search t
          ;; required by evil-collection
          evil-want-keybinding nil)
    (setq evil-emacs-state-tag (propertize " <E> " 'face '((:foreground "red"))))
    :config
    (evil-mode t)
    ;; Make :x :q :wq close buffer instead of closing window
    (evil-define-command evil-quit (&optional force)
      "Kill current buffer."
      :repeat nil
      (interactive "<!>")
      (kill-current-buffer))
    (evil-ex-define-cmd "bd[elete]" #'kill-current-buffer)
    ;; mouse:
    ;; 1. disable drag to visual mode
    (evil-define-key 'motion 'global
      [down-mouse-1] nil)
    (evil-define-key nil 'global
      [down-mouse-1] nil
      [drag-mouse-1] nil
      ;; control-mouse scroll
      [C-mouse-4] nil
      [C-mouse-5] nil)
    ;; other general keybindings
    (evil-define-key nil 'global
      (kbd "C-:") #'execute-extended-command
      (kbd "C-S-v") #'yank)
    (evil-define-key 'normal 'global
      (kbd "C-l") #'evil-ex-nohighlight
      (kbd "Q") "@q"
      (kbd "U") #'evil-redo)

    (defun my/move-buffer-to-window (dir)
      "Move current buffer to another window in DIR.
Switch current window to previous buffer (if any)."
      (require 'windmove)
      (let ((other-win (windmove-find-other-window dir))
            (buf (current-buffer))
            (pt (point))
            (win-start (window-start)))
        (when (and other-win
                   buf
                   (not (window-minibuffer-p other-win)))
          (switch-to-prev-buffer)  ;; ignore error
          (select-window other-win)
          (switch-to-buffer buf)
          (set-window-start other-win win-start)
          (goto-char pt))))
    (defun my/move-buffer-to-window-left () (interactive) (my/move-buffer-to-window 'left))
    (defun my/move-buffer-to-window-right () (interactive) (my/move-buffer-to-window 'right))
    (defun my/move-buffer-to-window-up () (interactive) (my/move-buffer-to-window 'up))
    (defun my/move-buffer-to-window-down () (interactive) (my/move-buffer-to-window 'down))

    (evil-define-key 'normal 'global
      (kbd "C-w C-h") #'my/move-buffer-to-window-left
      (kbd "C-w C-j") #'my/move-buffer-to-window-down
      (kbd "C-w C-k") #'my/move-buffer-to-window-up
      (kbd "C-w C-l") #'my/move-buffer-to-window-right))

  (use-package evil-collection
    :demand t
    :after evil
    :config
    ;; remove keybindings for some modes. let's do them on our own
    (mapc (lambda (x) (setq evil-collection-mode-list (delete x evil-collection-mode-list)))
          '(vterm company corfu wdired))
    (setq evil-collection-want-unimpaired-p nil)
    (evil-collection-init))

  (use-package evil-commentary
    :after evil
    :delight evil-commentary-mode
    ;; do not use hook prog-mode
    ;; because some modes (e.g. conf-mode) has comment support but is not prog-mode
    :demand t
    :config (evil-commentary-mode t))

  (use-package evil-surround
    :demand t
    :after evil
    :config (global-evil-surround-mode t))

  (use-package vimish-fold)

  ;; evil-vimish-fold would automatically call vimish-fold-mode
  (use-package evil-vimish-fold
    :delight evil-vimish-fold-mode
    :init (evil-define-key 'normal 'global
            ;; refresh marks
            (kbd "z g") #'vimish-fold-from-marks)
    ;; do not enable for remote files, because it would block while persisting the folding
    :hook (prog-mode-local-only . evil-vimish-fold-mode))

  (use-package evil-owl
    :demand t
    :custom (evil-owl-idle-delay 0.5)
    :delight evil-owl-mode
    :config
    (setq evil-owl-max-string-length 500)
    (add-to-list 'display-buffer-alist
                 '("*evil-owl*"
                   (display-buffer-in-side-window)
                   (side . bottom)
                   (window-height . 0.3)))
    ;; it's a global mode
    (evil-owl-mode))

  (use-package evil-snipe
    :demand t
    :after evil
    ;; :custom
    ;; (evil-snipe-scope 'visible)
    :delight evil-snipe-local-mode
    :config
    ;; Only uses its "clever-f" feature, not its two-char jump feature; use avy-mode for that
    ;; global mode
    ;; (evil-snipe-mode)
    (evil-snipe-override-mode))

  (use-package avy
    :after evil
    :custom (avy-background t)
    :init (evil-define-key 'normal 'global
            (kbd "s") #'avy-goto-char-2))

  (use-package evil-visualstar
    :demand t
    :after evil
    :config (global-evil-visualstar-mode))

  (when (my/macos-p)
    ;; (setq mac-command-modifier 'super
    ;;       mac-option-modifier 'meta)
    ;; Use command as control here, like (my modified) linux
    (setq mac-command-modifier 'control
          mac-control-modifier 'meta
          mac-option-modifier 'super)
    (setq mac-pass-command-to-system nil)
    ;; mimic the linux i3 keybindings
    (evil-define-key nil 'global
      (kbd "s-h") #'evil-window-left
      (kbd "s-j") #'evil-window-down
      (kbd "s-k") #'evil-window-up
      (kbd "s-l") #'evil-window-right
      (kbd "s-Q") #'save-buffers-kill-emacs))

  ) ;; }}}

(progn  ;; Some essential utils {{{
  (use-package switch-buffer-functions
    :demand t)
  (use-package exec-path-from-shell
    :when (my/macos-p)
    :demand t
    :config
    (exec-path-from-shell-initialize))
  (use-package add-node-modules-path
    :hook (js-mode . add-node-modules-path))
 (use-package fringe-scale
   :demand t
   :unless (my/macos-p)
   :init (setq fringe-scale-width my/gui-fringe-size)
   :config (fringe-scale-setup))

  (use-package which-key
    :demand t
    :delight which-key-mode
    :custom (which-key-ellipsis "..")  ;; see `truncate-string-ellipsis'
    :config (which-key-mode t))

  (use-package help-fns  ;; the builtin package
    :init
    (evil-define-key 'normal 'global
      (kbd "C-h F") #'describe-face
      (kbd "C-h C-k") #'describe-keymap)
    :config
    ;; This is to prevent showing empty string in completing-read, caused by llama ("##" symbol)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html
    ;; https://git.sr.ht/~tarsius/llama
    ;; WTF
    (my/define-advice help--symbol-completion-table (:filter-args (args) filter-out-empty-string)
      (when-let ((pred (nth 1 args)))
        (when (functionp pred)
          (let ((new-pred (lambda (x) (and (funcall pred x) (not (string= x ""))))))
            (setf (nth 1 args) new-pred))))
      args))

  (use-package info
    :config
    (evil-define-key 'normal Info-mode-map
      (kbd "C-t") nil))

  )  ;; }}}


(progn  ;; Theme {{{
  (defvar my/monoink nil "MonoInk mode")
  (setq my/monoink (equal (getenv "EMACS_MONOINK") "1"))

  (defun my/monoink-refresh (&rest _)
    (when my/monoink
      (call-process "killall" nil nil nil "-USR1" "rabbitink")))

  (defun my/monoink-refresh-on-window-config-change (&rest _)
    (when my/monoink
      (unless (or (minibuffer-window-active-p (minibuffer-window))
                  (which-key--popup-showing-p)
                  (get-buffer "*evil-owl*")
                  (eq last-command 'evil-write)
                  ;; triggered by child frame (e.g. posframe)
                  (frame-parent (selected-frame)))
        (run-with-timer 0 nil #'my/monoink-refresh))))
  (add-hook 'window-configuration-change-hook #'my/monoink-refresh-on-window-config-change)

  (when my/monoink
    (my/define-advice display-color-p (:override (&rest _) monoink-no-color)
      nil))

  (my/define-advice load-theme (:after (&rest _) patch-term-color-black)
    ;; Fix term-color-black:
    ;; By default, term-color-black is base02 (see solarized-faces.el),
    ;; which is, by its definition, a background color (very light in solarized-light).
    ;; However, shells don't need it as background, but instead they would use it to render text:
    ;; e.g. in xonsh, this color is used for displaying aborted commands and suggestions,
    ;; which should be a "comment"-like foreground color, which is base01
    (let ((color (face-foreground 'shadow)))
      (custom-set-faces
       `(term-color-black ((t (:foreground ,color :background ,color)))))))

  (defun my/load-single-theme (theme)
    "Load (and enable) single theme, disable all others."
    (interactive (list (completing-read "Theme: " (custom-available-themes) nil t)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) 'no-confirm))
  (evil-define-key 'normal 'global (kbd "C-x -") #'my/load-single-theme)

  (use-package solarized-theme
    :demand t
    :custom
    (solarized-use-variable-pitch nil)
    (solarized-use-more-italic t)
    ;; (solarized-emphasize-indicators nil)  ;; this will remove the flycheck fringe background
    )

  (if my/monoink
      (load-theme 'monoink t)
    (load-theme 'solarized-light t))

  (setq-default mode-line-format
                (delete '(vc-mode vc-mode) mode-line-format))
  (custom-set-faces
   '(fixed-pitch ((t (:family nil :inherit default))))
   '(fixed-pitch-serif ((t :family nil :slant italic :inherit default)))
   '(line-number ((t (:height 0.9))))  ;; for pragmata, there's no light weight, let's use a smaller size
   '(mode-line ((t (:height 0.9))))  ;; smaller mode-line
   '(mode-line-inactive ((t (:background nil :inherit mode-line)))))

  ;; Font size management
  (defun my/change-font-size ()
    "Change font size based on predefined list"
    (interactive)
    (when-let* ((size-str (completing-read
                           "Select font size:"
                           (mapcar #'number-to-string my/gui-font-size-choices)
                           nil nil nil t
                           (number-to-string (car (remove my/gui-font-size-current my/gui-font-size-choices)))))
                (size-val (string-to-number size-str)))
      (when (> size-val 0)
        (my/gui-font-size-set size-val))))
  ;; NOTE: there's no way to implement auto-changing function
  ;; because my external monitor shares the same resolution with my laptop monitor
  (evil-define-key nil 'global
    (kbd "C-x =") #'my/change-font-size)

  ;; (add-to-list 'face-font-rescale-alist '(".*CJK.*" . 0.75))
  )  ;; }}}

(progn  ;; minibuffer completion {{{

  (use-package recentf
    :config
    (defun my/recentf-keep-predicate (file)
      (and (not (file-remote-p file))
           (file-readable-p file)))
    (setq recentf-keep '(my/recentf-keep-predicate)))

  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion))))
    :config
    ;; https://github.com/minad/vertico/blob/0831da48fe75a173a27eb1ff2837777c80f0a2f4/vertico.el#L296
    ;; https://github.com/minad/vertico/issues/27#issuecomment-1057924544
    (let ((orig-fn (symbol-function 'orderless-highlight-matches)))
      (defun my/orderless-reset-function ()
        (fset 'orderless-highlight-matches orig-fn))
      (add-hook 'minibuffer-setup-hook #'my/orderless-reset-function)))

  (use-package vertico
    :demand t
    :custom
    (vertico-sort-function nil)
    :config
    (vertico-mode)
    (define-key vertico-map (kbd "C-j") #'vertico-next)  ;; originally C-n
    (define-key vertico-map (kbd "C-k") #'vertico-previous)
    (define-key vertico-map (kbd "C-n") #'next-history-element)  ;; originally M-n
    (define-key vertico-map (kbd "C-p") #'previous-history-element)
    ;; `vertico-exit-input' default is "M-RET". It's used to submit empty string.
    ;; https://github.com/minad/vertico#submitting-the-empty-string
    (define-key vertico-map (kbd "<C-return>") #'vertico-exit-input))

  (use-package vertico-directory
    :after vertico
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  ;; (use-package marginalia
  ;;   :demand t
  ;;   :config (marginalia-mode))

  (use-package consult
    :custom
    (consult-project-function #'projectile-project-root)
    ;; xref
    (xref-show-xrefs-function #'consult-xref)
    (xref-show-definitions-function #'consult-xref)
    :hook (minibuffer-setup . my/setup-consult-completion-in-minibuffer)
    :init
    (evil-define-key '(insert emacs normal motion) 'global
      (kbd "C-t") #'my/consult-buffer-vterm-only
      (kbd "C-r") #'consult-buffer)
    (evil-define-key 'normal 'global
      (kbd "g s") #'consult-imenu  ;; LSP would integrate with imenu to provide file symbols
      (kbd "g S") #'consult-imenu-multi
      (kbd "C-h i") #'consult-info
      (kbd "C-/") #'my/consult-line
      (kbd "C-?") #'consult-ripgrep)
    :commands (my/consult-buffer-vterm-only my/consult-line)
    :config
    (recentf-mode 1)

    (defun my/setup-consult-completion-in-minibuffer ()
      (setq-local completion-in-region-function #'consult-completion-in-region))

    (setq consult-ripgrep-args (string-replace " --search-zip" "" consult-ripgrep-args))

    ;; modifications for consult-grep / consult-ripgreps
    ;; support "C-d" to interactive change directory while grepping
    (defvar my/consult-grep-want-change-directory nil)
    (defvar my/consult-grep-last-input nil)
    (defun my/consult-grep-quit-to-change-directory ()
      (interactive)
      (setq my/consult-grep-want-change-directory t
            my/consult-grep-last-input (minibuffer-contents-no-properties))
      (exit-minibuffer))
    (defvar-keymap my/consult-grep-keymap
      "C-d" #'my/consult-grep-quit-to-change-directory)

    (add-to-list 'consult--customize-alist '(consult-ripgrep :keymap my/consult-grep-keymap))
    (add-to-list 'consult--customize-alist '(consult-grep :keymap my/consult-grep-keymap))
    (add-to-list 'consult--customize-alist '(consult-git-grep :keymap my/consult-grep-keymap))

    (my/define-advice consult--grep (:around (old-fn prompt make-builder dir initial) loop-allow-change-dir)
      "Change `consult--grep' into a loop, allowing to quit and change directory and try again."
      (let ((orig-this-command this-command))
        (catch 'break
          (while t
            (setq my/consult-grep-want-change-directory nil
                  my/consult-grep-last-input nil
                  this-command orig-this-command)
            (funcall old-fn prompt make-builder dir initial)
            (unless (and my/consult-grep-want-change-directory
                         (setq initial (when (length> my/consult-grep-last-input 1)
                                         (substring my/consult-grep-last-input 1))
                               dir (read-directory-name "Change directory: " dir))
                         (length> dir 0))
              (throw 'break nil))))))

    ;; consult buffers
    (setq my/consult--source-vterm-buffer
          `(
            :name "VTerm"
            :narrow ?t
            :category buffer
            :face consult-buffer
            :state ,#'consult--buffer-state
            :annotate ,(lambda (cand)
                         (let ((buf (get-buffer cand)))
                           (buffer-local-value 'default-directory buf)))
            :items ,(lambda () (consult--buffer-query :sort 'visibility
                                                      :as #'buffer-name
                                                      :include (list (rx bos "%vterm"))))))
    (setq my/consult--source-buffer
          `(
            :name "Buffer"
            :narrow ?b
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :default t
            :annotate ,(lambda (cand)
                         (let ((buf (get-buffer cand)))
                           (when (buffer-file-name buf)
                             (buffer-local-value 'default-directory buf))))
            :items ,(lambda () (consult--buffer-query :sort 'visibility
                                                      :as #'buffer-name
                                                      :exclude (cons (rx bos "%vterm") consult-buffer-filter)))))

    (delete 'consult--source-bookmark consult-buffer-sources)
    (delete 'consult--source-buffer consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'my/consult--source-buffer)

    (defun my/consult-buffer-vterm-only ()
      (interactive)
      (let ((consult-buffer-sources '(my/consult--source-vterm-buffer))
            (consult--buffer-display
             (lambda (buffer-name &optional norecord)
               (if-let ((buf (get-buffer buffer-name)))
                   (switch-to-buffer buf norecord)
                 (message "Buffer `%s' does not exists. Maybe vterm title changed?" buffer-name)))))
        (consult-buffer)))


    (defvar my/consult-line-should-add-to-evil-history nil)
    (defun my/consult-line-mark-add-to-evil-history-and-return ()
      (interactive)
      (setq my/consult-line-should-add-to-evil-history t)
      (call-interactively (key-binding (kbd "RET"))))
    (defvar-keymap my/consult-line-keymap
      "C-<return>" #'my/consult-line-mark-add-to-evil-history-and-return)
    (add-to-list 'consult--customize-alist '(consult-line :keymap my/consult-line-keymap))
    (add-to-list 'consult--customize-alist '(my/consult-line :keymap my/consult-line-keymap))

    ;; from https://github.com/minad/consult/issues/318#issuecomment-882067919
    ;; modified so that only triggers when pressing C-RET
    (defun my/consult-line ()
      (interactive)
      (setq my/consult-line-should-add-to-evil-history nil)
      (call-interactively #'consult-line)
      (when (and my/consult-line-should-add-to-evil-history
                 (bound-and-true-p evil-mode)
                 (eq evil-search-module 'evil-search))
        (when-let ((pattern (car (orderless-pattern-compiler (car consult--line-history)))))
          (add-to-history 'evil-ex-search-history pattern)
          (setq evil-ex-search-pattern (list pattern t t))
          (setq evil-ex-search-direction 'forward)
          (when evil-ex-search-persistent-highlight
            (evil-ex-search-activate-highlight evil-ex-search-pattern))))))

  (use-package embark
    :custom
    (embark-mixed-indicator-delay 0.5)
    :init
    (evil-define-key '(normal motion emacs) 'global
      (kbd "C-.") #'embark-act)
    ;; evil-define-key does not work on minibuffer
    :bind (("C-." . embark-act))
    :config
    (define-key embark-url-map
                "B" #'browse-url-default-browser)  ;; external browser
    (define-key embark-general-map
                "q" #'keyboard-quit))

  (use-package embark-consult
    :after (embark consult)
    :demand t)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq completions-detailed t
        enable-recursive-minibuffers t)

  )  ;; }}}

(progn  ;; Editing-related settings {{{
  (add-hook 'prog-mode-hook
            (lambda () (modify-syntax-entry ?_ "w")))

  ;; disable backup. put autosaves into .emacs.d/autosave
  (setq make-backup-files nil
        auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/\\1" t)))
  ;; disblae lockfiles ( .#xxx file )
  (setq create-lockfiles nil)

  (custom-set-variables
   ;; default modes
   '(indent-tabs-mode nil)
   '(line-number-mode nil)
   '(blink-cursor-mode nil)
   '(tool-bar-mode nil)
   '(save-place-mode t)
   '(size-indication-mode nil)
   '(scroll-margin 2)
   ;; editing
   '(truncate-lines t)
   '(tab-always-indent nil)
   '(tab-width 4))

  (unless my/monoink
    (custom-set-variables
     '(scroll-conservatively 101)
     '(hscroll-step 1)
     '(scroll-step 1)))

  (setq redisplay-skip-fontification-on-input t)

  (my/define-advice make-auto-save-file-name (:around (old-fn &rest args) shorten)
    "Shorten filename using hash function so that it will not be too long."
    (let ((buffer-file-name
           (when buffer-file-name (sha1 buffer-file-name))))
      (apply old-fn args)))

  )  ;; }}}

(progn  ;; Builtin editing-related packages: whitespace, hl-line, ... {{{
  (use-package outline
    :delight outline-minor-mode)

  (use-package whitespace
    :hook (prog-mode . whitespace-mode)
    :delight whitespace-mode
    :custom (whitespace-style '(face trailing indentation space-after-tab space-before-tab tab-mark))
    :custom-face (whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :inherit whitespace-space)))))

  (use-package hl-line
    :unless my/monoink
    :hook
    (prog-mode . hl-line-mode)
    (tabulated-list-mode . hl-line-mode))

  (use-package display-line-numbers
    :hook (prog-mode . display-line-numbers-mode))

  (use-package elec-pair
    :init (setq electric-pair-skip-whitespace nil)
    :hook (prog-mode . electric-pair-local-mode))

  (use-package paren
    :demand t
    :init (setq show-paren-when-point-inside-paren t
                show-paren-context-when-offscreen 'overlay)
    :config (show-paren-mode t))

  (use-package autorevert
    :demand t
    :delight auto-revert-mode
    :custom (auto-revert-avoid-polling t)
    :config
    (global-auto-revert-mode t))

  (use-package eldoc
    ;; delight
    :init (setq eldoc-minor-mode-string nil))

  (use-package tramp
    :config (setq vc-ignore-dir-regexp
                  (format "\\(%s\\)\\|\\(%s\\)"
                          locate-dominating-stop-dir-regexp
                          tramp-file-name-regexp)))

  (use-package abbrev
    :custom (save-abbrevs nil)
    :hook (prog-mode . my/disable-abbrev-mode)
    :demand t
    :config
    ;; abbrev-mode is only used for expanding the abbrev automatically without confirming
    ;; and it's enabled by some modes (e.g. cc-mode) automatically.
    ;; let's disable it.
    (defun my/disable-abbrev-mode ()
      "Disable abbrev-mode."
      (when abbrev-mode
        (abbrev-mode -1))))

  (use-package image-dired
    :custom (image-dired-thumbnail-storage 'standard))

  (use-package wdired
    :config
    (my/define-advice wdired-change-to-wdired-mode (:after (&rest _) enter)
      (highlight-changes-mode 1)
      (delight-major-mode))
    (my/define-advice wdired-change-to-dired-mode (:around (fn &rest args) leave)
      (highlight-changes-mode -1)
      (apply fn args)
      (delight-major-mode)))

  ) ;; }}}

(progn ;;; {{{  Buffer management
  (use-package midnight  ;; builtin
    :demand t
    :custom
    (midnight-delay "4:00am")
    (clean-buffer-list-kill-regexps
     `(,(rx eos "*" (or "Man" "WoMan") " ")
       ,(rx eos "magit" (* anything) ":")))
    (clean-buffer-list-kill-buffer-names
     '("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*Ibuffer*" "*Async-native-compile-log*")))

  (use-package ibuffer  ;; builtin
    :custom
    (ibuffer-default-sorting-mode 'filename/process)
    ;; replace buffer-menu with ibuffer for evil :ls
    :init (evil-ex-define-cmd "ls" #'ibuffer))

  )  ;;; }}}

(progn  ;; Editing-related packages: indent, git-gutter, .. {{{

  ;; git-gutter is orphan now, and diff-hl is prefered.
  ;; however, I want to use git-gutter's face and fringe style.
  ;; So here it is: using diff-hl's logic, and git-gutter's style
  (use-package git-gutter-fringe)

  (use-package diff-hl
    :hook (prog-mode-local-only . diff-hl-mode)
    :custom
    (diff-hl-draw-borders nil)
    :custom-face
    (diff-hl-insert ((t (:foreground unspecified :background unspecified :inherit git-gutter-fr:added))))
    (diff-hl-delete ((t (:foreground unspecified :background unspecified :inherit git-gutter-fr:deleted))))
    (diff-hl-change ((t (:foreground unspecified :background unspecified :inherit git-gutter-fr:modified))))
    :config
    (require 'git-gutter-fringe)

    (defun my/diff-hl-fringe (type _pos)
      (pcase type
        ('insert 'git-gutter-fr:added)
        ('delete 'git-gutter-fr:deleted)
        ('change 'git-gutter-fr:modified)
        (_ 'question-mark)))
    (setq diff-hl-fringe-bmp-function #'my/diff-hl-fringe))

  (use-package rainbow-mode
    :hook ((html-mode tsx-ts-mode css-mode) . rainbow-mode))

  (use-package hl-todo
    :delight hl-todo-mode
    :config
    (when my/monoink
      (setq hl-todo-keyword-faces
            (mapcar (lambda (key-color) (cons (car key-color) "black")) hl-todo-keyword-faces)))
    :hook (prog-mode . hl-todo-mode))

  (use-package dtrt-indent
    :delight dtrt-indent-mode
    :hook (prog-mode . dtrt-indent-mode)
    :config
    (add-to-list 'dtrt-indent-hook-mapping-list
                 '(cmake-mode default cmake-tab-width)))
  )  ;; }}}

(progn  ;; Auto-insert & snippets {{{

  (use-package tempel
    :custom
    (tempel-trigger-prefix "~")
    (tempel-auto-reload nil)  ;; by default, it would check the file last-modified-time on each completion
    (tempel-path (list (expand-file-name "templates" user-emacs-directory)
                       (expand-file-name "templates.custom/*.eld" user-emacs-directory)))
    :hook ((prog-mode . my/tempel-setup-capf)
           (pr-review-input-mode . my/tempel-setup-capf)
           ;; NOTE: lsp-completion-mode would add its own CAPF function, but we want to make sure that this is the first, so hook to lsp-mode-hook
           (lsp-completion-mode . my/tempel-setup-capf))
    :bind (:map tempel-map
                ("<tab>" . tempel-next)
                ("<backtab>" . tempel-previous))
    :commands (my/tempel-reload)
    :config
    (add-hook 'evil-insert-state-exit-hook #'tempel-done)  ;; deactivate tempel regions
    (defun my/tempel-expand ()
      "A slightly modified version of `tempel-expand', with the following difference:
1. do not repsect `tempel-trigger-prefix'. because I want that to only affect `tempel-complete'.
2. return the completion table with ~ suffix. so that company-mode would display the tooltip so that it can be expanded."
      (when-let* ((templates (tempel--templates))
                  (bounds (bounds-of-thing-at-point 'symbol))
                  (name (buffer-substring-no-properties (car bounds) (cdr bounds)))
                  (sym (intern-soft name))
                  (template (assq sym templates)))
        (setq templates (list (cons (intern (concat name "~")) (cdr template))))
        (list (car bounds) (cdr bounds)
              (tempel--completion-table templates)
              :exclusive 'no
              :exit-function (apply-partially #'tempel--exit templates nil))))
    (defun my/tempel-setup-capf ()
      "Add both `my/tempel-expand' and `tempel-complete' (in order) to `completion-at-point-functions'"
      (make-local-variable 'completion-at-point-functions)
      ;; delete and add, to make sure it's in the first position
      (setq completion-at-point-functions (delete #'tempel-complete completion-at-point-functions))
      (setq completion-at-point-functions (delete #'my/tempel-expand completion-at-point-functions))
      (add-to-list 'completion-at-point-functions #'tempel-complete)
      (add-to-list 'completion-at-point-functions #'my/tempel-expand))
    (defun my/tempel-reload ()
      (interactive)
      (setq tempel--path-templates nil)
      (message "Reloaded templates")))

  ;; NOTE: autoinsert used `skeleton-insert' syntax;
  ;; tempel used `tempo-insert' syntax;

  (use-package autoinsert
    :delight auto-insert-mode
    ;; same as `auto-insert-mode' (global mode)
    ;; cannot add to major-mode-hook, because some snippet needs buffer local variables (which is load after major mode hook)
    :hook (find-file . auto-insert)
    :init
    (defvar my/snippet-copyright-lines nil
      "Lines of copyright header in snippet. Maybe a list of strings or a function that generate a list of strings.")
    :config
    ;; filter `auto-insert-alist', only keep some of them
    (defun my/keep-auto-insert-entry-p (entry)
      "Check to keep ENTRY in `auto-insert-alist'"
      (let (filename name)
        (if (consp (car entry))
            (setq filename (caar entry)
                  name (cdar entry))
          (setq filename (car entry)))
        (or (member filename '(".dir-locals.el"))
            (member name '("Emacs Lisp header")))))
    (setq auto-insert-alist
          (seq-filter #'my/keep-auto-insert-entry-p auto-insert-alist))

    (defun my/snippet-copyright-as-comment ()
      "Return copyright as comment string for current buffer."
      (when-let ((lines (if (functionp my/snippet-copyright-lines)
                            (funcall my/snippet-copyright-lines)
                          my/snippet-copyright-lines)))
        (concat (mapconcat (lambda (line) (concat comment-start " " line comment-end))
                           lines "\n")
                "\n\n")))

    (define-auto-insert `(,(rx "." (or "h" "hpp" "hh") eos) . "C++ Header")
      '(nil (my/snippet-copyright-as-comment) "#pragma once\n"))

    (define-auto-insert `(,(rx "." (or "c" "cc" "cpp") eos) . "C++ Source")
      '(nil (my/snippet-copyright-as-comment)
            "#include \""
            (let* ((root (or (projectile-project-root) default-directory))
                   (rel (file-relative-name buffer-file-name root)))
              (file-name-with-extension rel ".h"))
            "\"\n"))

    (define-auto-insert `(,(rx ".proto" eos) . "Proto2")
      '(nil "syntax = \"proto2\";\n\n"
            "package " _ ";\n"))

    (define-auto-insert `(,(rx ".py" eos) . "Python 3")
      '(nil "#!/usr/bin/env python3\n# -*- coding: utf-8 -*-\n\n"
            (my/snippet-copyright-as-comment)))

    (define-auto-insert `(,(rx ".pl" eos) . "Perl")
      '(nil "#!/usr/bin/env perl\n\nuse v5.35;\nuse strict;\nuse warnings;\n\n"))

    )
  )  ;; }}}

(progn  ;; Filetypes (Major modes)  {{{
  (defun my/ensure-prog-mode ()
    "Run `prog-mode-hook' and related settings.
Useful for modes that does not derive from `prog-mode'."
    (unless (derived-mode-p 'prog-mode)
      (setq-local require-final-newline t)
      (run-hooks 'prog-mode-hook)))

  (use-package conf-mode
    :mode (rx word-boundary "OWNERS" eos)
    :config (add-hook 'conf-mode-hook #'my/ensure-prog-mode))

  (use-package cmake-mode)

  (use-package fish-mode)

  (use-package vimrc-mode)

  (use-package jinja2-mode
    :config (add-hook 'jinja2-mode-hook #'my/ensure-prog-mode))

  (use-package protobuf-mode
    :config (add-hook 'protobuf-mode-hook #'my/ensure-prog-mode))

  (use-package gn-mode
    :mode (rx ".gn" (? "i") eos))

  (use-package bazel
    ;; https://github.com/bazelbuild/emacs-bazel-mode/issues/122
    :mode ((rx ".BUILD" eos) . bazel-build-mode)
    :custom (bazel-mode-buildifier-before-save t))

  (use-package yaml-mode
    :mode (rx ".y" (? "a") "ml" eos)
    :config (add-hook 'yaml-mode-hook #'my/ensure-prog-mode))

  (use-package kotlin-mode)

  (use-package groovy-mode)

  (use-package xonsh-mode)

  (use-package markdown-mode
    :init (setq markdown-command "markdown2"))

  (use-package go-mode)

  ;; built-in javascript-mode supports .js and .jsx

  (use-package lua-mode)

  (use-package haskell-mode)

  (use-package jsonnet-mode
    :hook (jsonnet-mode . my/jsonnet-mode-setup)
    :config
    (defun my/jsonnet-mode-setup()
      "Setup for `jsonnet-mode'."
      ;; Disable jsonnet flycheck because it's not working right and produces annoying results
      (setq flycheck-disabled-checkers '(jsonnet))))

  (use-package dockerfile-mode)

  (use-package bpftrace-mode)

  (use-package just-mode)

  (use-package rust-mode)

  (use-package ebuild-mode)

  (use-package cuda-mode
    :config (add-hook 'cuda-mode-hook #'my/ensure-prog-mode))

  (add-to-list 'auto-mode-alist `(,(rx ".mm" eos) . objc-mode))
  ;; for Weixin Mini Program
  (add-to-list 'auto-mode-alist `(,(rx ".wxml" eos) . nxml-mode))
  (add-to-list 'auto-mode-alist `(,(rx ".wxss" eos) . css-mode))

  (setq python-prettify-symbols-alist '())

  ;; CC mode
  (use-package google-c-style
    :demand t
    :config (c-add-style "Google" google-c-style))

  (custom-set-variables
   '(c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (other . "google")))
   '(c-tab-always-indent nil))

  ;; GOLANG
  (defun my/go-install-save-hooks ()
    "Install save hooks for go."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'my/go-install-save-hooks)
  ) ;;; }}}

(when (>= emacs-major-version 29)  ;; Tree-sitter {{{

  ;; only for auto installing. do not enable those modes automatically
  (use-package treesit-auto
    :commands my/treesit-install-all
    :config
    (defun my/treesit-install-all ()
      "Install all treesit libs."
      (interactive)
      (treesit-auto-install-all)))

  (use-package c-ts-mode
    :config
    ;; fix delight
    (my/define-advice c-ts-mode-set-modeline (:override (&rest _) fix-delight)
      nil))

  ;; NOTE: it's unbelievable that running *-ts-mode would alter `auto-mode-alist' globally
  ;; so, to avoid running those accidentally (either interactively or by other packages),
  ;; remap those back to normal modes.
  (setq major-mode-remap-alist '((c++-ts-mode . c++-mode)
                                 (c-ts-mode . c-mode)
                                 (c-or-c++-ts-mode . c-or-c++-mode)
                                 (python-ts-mode . python-mode)
                                 (go-ts-mode . go-mode)
                                 (rust-ts-mode . rust-mode)))

  (use-package typescript-ts-mode
    :mode
    ((rx ".ts" eos) . typescript-ts-mode)
    ((rx ".tsx" eos) . tsx-ts-mode)
    :hook (typescript-ts-base-mode . my/typescript-mode-set-lsp-settings)
    :config
    (defun my/typescript-mode-set-lsp-settings ()
      "Set lsp settings for typescript mode."
      ;; tsserver returns markdown doc for eldoc
      ;; which requires lsp-eldoc-render-all to be fully shown
      (setq-local lsp-eldoc-render-all t)))

  )  ;;; }}}

(progn  ;; ORG mode {{{
  (use-package org
    :custom
    (org-directory "~/Notes/org/")
    (org-mobile-directory org-directory)
    (org-agenda-files '("~/Notes/org/"))
    (org-default-notes-file "~/Notes/org/inbox.org")
    (org-mobile-inbox-for-pull "~/Notes/org/inbox.org")
    (org-capture-templates `(("c" "Inbox" entry
                              (file "inbox.org")
                              ,(concat
                                "* %?\n"
                                "%U\n"
                                "%i")
                              :prepend t
                              :empty-lines-after 1)))
    ;; more content-specific settings should go to .dir-locals.el or init.org in ~/Notes/org/
    :mode ((rx ".org" eos) . org-mode)
    :bind (("C-c o l" . org-store-link)
           ("C-c o a" . org-agenda)
           ("C-c o c" . org-capture)
           ("C-c o o" . my/find-file-in-org-directory))
    :config
    (evil-define-key 'normal org-mode-map
      (kbd "RET") #'org-open-at-point)

    (defun my/find-file-in-org-directory ()
      (interactive)
      (let ((default-directory org-directory))
        (call-interactively #'find-file)))

    (defun my/confirm-org-mobile-push ()
      (unless (yes-or-no-p "Really run `org-mobile-push'?")
        (error "Abort")))
    (add-hook 'org-mobile-pre-push-hook #'my/confirm-org-mobile-push))

  (use-package org-tree-slide
    :after org
    :init
    (evil-define-key 'normal org-mode-map
      (kbd "<f8>") 'org-tree-slide-mode
      (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
    (evil-define-minor-mode-key 'normal 'org-tree-slide-mode
      (kbd "{") #'org-tree-slide-move-previous-tree
      (kbd "}") #'org-tree-slide-move-next-tree))
  )  ;;; }}}

(progn  ;; VTerm {{{
  (use-package with-editor
    :commands with-editor)
  (use-package vterm
    :demand t
    :init
    (setq
     vterm-kill-buffer-on-exit t
     vterm-max-scrollback 10000
     vterm-min-window-width 40
     vterm-buffer-name-string "%%vterm %s"   ;; see my/consult--source-vterm-buffer
     vterm-shell (or (executable-find "xonsh") shell-file-name))
    (when my/monoink
      (setq vterm-term-environment-variable "xterm-mono"))
    :config
    (defun my/vterm-set-pwd (path)
      "Set default-directory"
      ;; default-directory should always ends with "/"
      (unless (string-suffix-p "/" path)
        (setq path (concat path "/")))
      ;; only set if we're still in vterm buffer
      ;; to workaround the prompt after find-file
      (when (and (eq major-mode 'vterm-mode))
        (setq default-directory path)))
    (add-to-list 'vterm-eval-cmds '("set-pwd" my/vterm-set-pwd))
    (add-to-list 'vterm-eval-cmds '("man" man))
    (add-to-list 'vterm-eval-cmds '("magit-status" magit-status))
    (add-to-list 'vterm-eval-cmds '("rg-run-raw" my/rg-run-raw))
    (add-to-list 'vterm-eval-cmds '("woman-find-file" woman-find-file-with-fallback))
    (add-to-list 'vterm-eval-cmds '("find-file" my/find-file-fallback-sudo))

    (defun my/vterm-eval-base64-json (b64)
      "Decode B64 as base64 encoded json array, then evaluate it as vterm cmds.
The first element of the array is a command in `vterm-eval-cmds', while the remaining is arguments.
This is used to solve the complex quoting problem while using vterm message passing."
      (let* ((input (json-parse-string (base64-decode-string b64) :array-type 'list))
             (cmd (car input))
             (args (cdr input)))
        (when-let ((cmd-f (cadr (assoc cmd vterm-eval-cmds))))
          (apply cmd-f args))))
    (add-to-list 'vterm-eval-cmds '("eval-base64-json" my/vterm-eval-base64-json))

    (evil-collection-vterm-setup)
    ;; do not modify evil-move-cursor-back.
    ;; when the cursor is at the EOL, the cursor would jump back on normal in either case
    ;; so it's better to keep it consistent
    (my/define-advice evil-collection-vterm-escape-stay (:override (&rest _) ignore)
      nil)

    (evil-set-initial-state 'vterm-mode 'insert)
    (evil-define-key nil vterm-mode-map
      (kbd "M-:") nil)
    (evil-define-key '(insert emacs) vterm-mode-map
      (kbd "C-a") 'vterm--self-insert
      (kbd "C-b") 'vterm--self-insert
      (kbd "C-c") 'vterm--self-insert
      (kbd "C-e") 'vterm--self-insert
      (kbd "C-d") 'vterm--self-insert
      (kbd "C-z") nil
      (kbd "C-w") 'vterm--self-insert
      (kbd "C-t") 'vterm--self-insert
      (kbd "C-p") 'vterm--self-insert
      (kbd "C-n") 'vterm--self-insert
      (kbd "C-j") 'vterm--self-insert
      (kbd "C-k") 'vterm--self-insert
      (kbd "C-x") 'vterm--self-insert
      (kbd "C-r") nil  ;; allow use C-r to find buffer in insert mode
      (kbd "C-S-v") 'vterm-yank)

    ;; https://github.com/emacs-evil/evil-collection/issues/651
    (defun my/evil-vterm-append ()
      (interactive)
      (vterm-goto-char (1+ (point)))
      (call-interactively #'evil-append))
    (evil-define-key 'normal vterm-mode-map
      (kbd "C-S-v") 'vterm-yank
      ;; Do not allow insertion commands in normal mode. Only allow "a"
      "a" #'my/evil-vterm-append
      "s" nil  ;; do not override avy keybindings
      [remap evil-open-below] #'ignore
      [remap evil-open-above] #'ignore
      [remap evil-join] #'ignore
      [remap evil-indent] #'ignore
      [remap evil-shift-left] #'ignore
      [remap evil-shift-right] #'ignore
      [remap evil-invert-char] #'ignore
      [remap evil-repeat] #'ignore)
    (evil-define-key 'emacs vterm-mode-map
      (kbd "<escape>") 'vterm--self-insert)
    ;; Must set default evil-*-state-cursor (and only once) before setting buffer-local variable
    ;; Cannot call it directly while initializing because there's no face-attribute in daemon mode
    (let ((my/vterm-setup-global-cursor-called nil))
      (defun my/vterm-setup-global-cursor (_)
        (unless my/vterm-setup-global-cursor-called
          (setq evil-normal-state-cursor `(box ,(face-attribute 'default :foreground))
                evil-insert-state-cursor `((bar . 2) ,(face-attribute 'default :foreground)))
          (setq my/vterm-setup-global-cursor-called t))))
    (defun my/vterm-init-custom ()
      ;; disable the default process-kill-buffer-query-function
      ;; see below my/vterm-process-kill-buffer-query-function
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (my/vterm-setup-global-cursor nil)
      ;; (make-local-variable 'evil-force-cursor)
      (make-local-variable 'evil-insert-state-cursor)
      (make-local-variable 'evil-normal-state-cursor)
      (setq evil-normal-state-cursor (if my/monoink '(box "gray40") '(box "red"))
            evil-insert-state-cursor `(box ,(face-attribute 'default :foreground)))
      ;; buffer-local evil hook, always reset cursor after entering insert state
      ;; https://github.com/emacs-evil/evil-collection/issues/651#issuecomment-1345505103
      (add-hook 'evil-insert-state-entry-hook #'vterm-reset-cursor-point nil t)
      (evil-refresh-cursor))
    (add-hook 'vterm-mode-hook #'my/vterm-init-custom)
    ;; needed by emacs 28. force refresh cursor after switching buffer
    (add-hook 'switch-buffer-functions
              (lambda (&rest _) (evil-refresh-cursor)))
    (defun my/with-editor-vterm ()
      (interactive)
      ;; this part is copied and simplified from with-editor
      ;; we don't want to use with-editor because it would add process filter
      ;; (for its fallback sleeping editor) which is slow
      (let ((process-environment process-environment)
            (default-directory default-directory))
        ;; this should be ensured; see the "server" section below
        (when (process-live-p server-process)
          (push (concat "EDITOR=emacsclient --socket-name="
                        (shell-quote-argument (expand-file-name server-name server-socket-dir)))
                process-environment))
        (when (file-remote-p default-directory)
          (setq default-directory "~/"))
        (vterm)))
    (evil-ex-define-cmd "term" #'my/with-editor-vterm)
    (evil-define-key '(normal motion emacs) 'global
      (kbd "<C-return>") #'my/with-editor-vterm)

    ;; (defun my/vterm-rename-buffer-as-title (title)
    ;;   (rename-buffer (format "vterm %s" title) t))
    ;; (add-hook 'vterm-set-title-functions
    ;;           #'my/vterm-rename-buffer-as-title)

    (defun my/vterm-clone-to-new-buffer ()
      "Clone the content of current vterm buffer to a new buffer.
Useful when I want to copy some content in history but a command is current running.
I don't want to use `vterm-copy-mode' because it pauses the terminal."
      (interactive)
      (let ((content (buffer-string))
            (pos (point))
            (newbuf (get-buffer-create (format "*vterm clone - %s*" (buffer-name)))))
        (with-current-buffer newbuf
          (insert content)
          (font-lock-mode 1)
          (add-function :filter-return  ;; for copying region with fake newline
                        (local 'filter-buffer-substring-function)
                        #'vterm--filter-buffer-substring)
          (setq-local header-line-format
                      (propertize "--- VTerm Cloned Buffer %-"
                                  'face '(:foreground "white" :background "red3")))
          (goto-char pos))
        (switch-to-buffer-other-window newbuf)))
    (evil-ex-define-cmd "vterm-clone" #'my/vterm-clone-to-new-buffer)
    (evil-define-key '(normal) vterm-mode-map
      (kbd "C-c C-c") #'my/vterm-clone-to-new-buffer)

    (defvar my/inhibit-startup-vterm nil
      "Non nil means that the startup vterm is already started, so we shoult inhibit startup vterm.")
    (my/define-advice display-startup-screen (:around (old-fn &rest args) start-vterm)
      "Around advice for `display-startup-screen' to start vterm at startup."
      (if my/inhibit-startup-vterm
          (apply old-fn args)
        (setq my/inhibit-startup-vterm t)
        (my/with-editor-vterm))))

  (defun my/vterm-process-kill-buffer-query-function ()
    (let* ((default-directory "/")  ;; avoid listing processes from remote host
           (process (get-buffer-process (current-buffer))))
      (or (not process)
          (not (eq major-mode 'vterm-mode))
          (not (memq (process-status process) '(run stop open listen)))
          ;; does not have any subprocess
          (not (member (process-id process)
                       (mapcar (lambda (p) (alist-get 'ppid (process-attributes p)))
                               (list-system-processes))))
          (yes-or-no-p (format "VTerm %S has a running subprocess; kill it? "
                               (buffer-name (current-buffer)))))))
  (defun my/vterm-process-kill-emacs-query-function ()
    (seq-every-p (lambda (buf)
                   (with-current-buffer buf
                     (my/vterm-process-kill-buffer-query-function)))
                 (buffer-list)))

  (add-hook 'kill-buffer-query-functions #'my/vterm-process-kill-buffer-query-function)
  (add-hook 'kill-emacs-query-functions #'my/vterm-process-kill-emacs-query-function)

  (my/define-advice vterm--redraw (:around (old-fn &rest args) keep-cursor-on-normal-mode)
    "Do not move cursor or window on redraw if not in evil insert mode."
    (if (or (evil-insert-state-p)
            ;; a simple heuristic check if cursor is at end, in which case keep scrolling even in normal mode.
            (< (- (point-max) (point)) 256))
        (apply old-fn args)
      (let ((pt (point)))
        (save-window-excursion  ;; vterm--redraw would call `recenter'
          (save-excursion
            (apply old-fn args)))
        (goto-char pt))))

  )  ;; }}}

(progn  ;; Project / Window management {{{
  (use-package projectile
    :init
    ;; Set "projectile-project-name" to override the name
    (defun my/projectile-mode-line ()
      "Modified version of projectile-default-mode-line"
      (format " @%s" (or (projectile-project-name) "-")))
    (setq projectile-completion-system 'default
          ;; NOTE: cache can be very big and consumes hundreds MBs of memory, which slows down GC
          projectile-enable-caching nil
          ;; I really want to disable cache. set `projectile-enable-caching' does not seem enough
          ;; also see advice below
          projectile-cache-file "/dev/null"
          ;; https://github.com/bbatsov/projectile/issues/1749
          projectile-generic-command "fd . -0 --type f --color=never --strip-cwd-prefix"
          projectile-switch-project-action #'projectile-dired
          projectile-mode-line-function #'my/projectile-mode-line
          ;; The following line is actually unused anymore
          projectile-mode-line-prefix " Proj")
    (autoload 'projectile-command-map "projectile" nil nil 'keymap)
    (evil-define-key '(normal motion emacs visual) 'global (kbd "C-p") 'projectile-command-map)
    :hook (prog-mode-local-only . projectile-mode)
    :commands projectile-project-root
    :config
    (projectile-mode t)
    (define-key projectile-command-map "F" 'projectile-find-file-other-window)
    (define-key projectile-command-map "h" 'projectile-find-other-file)
    (define-key projectile-command-map "H" 'projectile-find-other-file-other-window)
    (define-key projectile-command-map (kbd "C-<return>") 'projectile-run-vterm)

    (my/define-advice projectile-serialize-cache (:override () ignore)
      nil)

    (my/define-advice projectile-project-root (:before-while (&rest _) ignore-remote)
      (and default-directory
           (not (file-remote-p default-directory))))

    ;; Bridge projectile and project together so packages that depend on project
    ;; like eglot work
    (defun my/projectile-project-find-function (dir)
      (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root))))
    (with-eval-after-load 'project
      (add-to-list 'project-find-functions #'my/projectile-project-find-function)))

  (use-package winner
    :demand t
    :delight winner-mode
    :config
    (winner-mode t)
    (evil-define-key '(normal motion emacs) 'global
      (kbd "C-w u") 'winner-undo
      (kbd "C-w x") 'kill-this-buffer))
  )  ;; }}}

(progn  ;; completion {{{
  ;; the default value is 'tags-completion-at-point-functions
  ;; somehow it sometimes reports error when there's no TAGS file (even if tags-file-name is already set to buffer-local)
  ;; anyhow, we don't use TAGS anyway. disable it
  (setq-default completion-at-point-functions nil)

  ;; (use-package company-tabnine)

  (use-package company
    :init
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0  ;; default is 0.2
          company-abort-on-unique-match nil
          ;; NOTE: revert this if it's slow
          company-search-regexp-function 'company-search-flex-regexp
          company-tooltip-align-annotations t
          ;; show single candidate as tooltip
          company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
          company-backends '(company-files
                             ;; company-tabnine
                             company-capf
                             (company-dabbrev-code
                              ;; removed for slow performance
                              ;; company-gtags company-etags
                              company-keywords)
                             ;; company-dabbrev
                             ))

    (when my/monoink
      (setq company-format-margin-function 'company-text-icons-margin))
    :custom-face
    ;; preview is used for codeium below. use a comment-like face
    (company-preview ((t :foreground nil :background nil :inherit shadow)))
    :delight company-mode
    :hook ((prog-mode . company-mode)
           (pr-review-input-mode . company-mode)
           (comint-mode . company-mode)
           (git-commit-mode . company-mode)
           (org-mode . company-mode))
    :config
    ;; TODO: needs more improvement
    (evil-define-key 'insert comint-mode-map
      (kbd "<tab>") #'company-complete)
    ;; company-box is slow and old
    ;; (use-package company-box  ;; this does not hide line number while showing completions
    ;;   :init (setq company-box-show-single-candidate t
    ;;               company-box-doc-delay 1
    ;;               company-box-icons-alist 'company-box-icons-all-the-icons)
    ;;   :hook (company-mode . company-box-mode)
    ;;   :delight company-box-mode)
    (evil-define-key nil company-active-map
      ;; do not use company-select-(next-previous)-or-abort for these keys
      ;; because in company-preview frontend (codeium), we don't know if it would abort or not,
      ;; so it may end up invoking other actions
      (kbd "C-n") 'company-select-next
      (kbd "C-p") 'company-select-previous
      (kbd "C-j") 'company-select-next
      (kbd "C-k") 'company-select-previous
      (kbd "<tab>") 'company-complete-selection
      (kbd "C-f") 'company-complete-selection  ;; mostly useful for company-preview frontend in codeium
      ;; (kbd "<tab>") (lambda () (interactive) (company-complete-common-or-cycle 1))
      (kbd "RET") nil
      (kbd "<return>") nil
      (kbd "C-h") nil
      ;; the completion popup will not disappear while working with lsp and capf
      ;; https://github.com/emacs-lsp/lsp-mode/issues/1447
      (kbd "<escape>") (lambda () (interactive) (company-abort) (evil-normal-state)))
    (evil-define-key nil company-search-map
      (kbd "C-j") 'company-select-next
      (kbd "C-k") 'company-select-previous
      (kbd "<escape>") 'company-search-abort)

    ;; manual trigger
    (evil-define-minor-mode-key 'insert 'company-mode-map
      (kbd "C-j") 'company-complete
      (kbd "C-n") 'company-complete)
    ;; (company-tng-configure-default)

    ;; set `completion-styles' to default for company-capf
    ;; because we set orderless for minibuffer
    (require 'company-capf)
    (my/define-advice company-capf (:around (capf-fn &rest args) set-completion-styles)
      (let ((completion-styles '(basic partial-completion)))
        (apply capf-fn args))))

  (use-package company-emoji
    :after company
    :custom
    (company-emoji-insert-unicode nil))

  (comment corfu
    :init
    (setq corfu-auto t
          corfu-auto-prefix 1
          corfu-auto-delay 0
          corfu-sort-function nil
          corfu-preview-current nil)
    :hook ((prog-mode . corfu-mode)
           (pr-review-input-mode . corfu-mode)
           (git-commit-mode . corfu-mode)
           (minibuffer-mode . corfu-mode))
    :config
    (defun my/corfu-quit-and-escape ()
      "Call `corfu-quit' and then return to Normal State."
      (interactive)
      (call-interactively 'corfu-quit)
      (evil-normal-state))

    (evil-define-minor-mode-key 'insert 'corfu-map
      (kbd "C-n") 'corfu-next
      (kbd "C-p") 'corfu-previous
      (kbd "C-j") 'corfu-next
      (kbd "C-k") 'corfu-previous
      (kbd "<escape>") 'my/corfu-quit-and-escape)
    (evil-define-key nil corfu-map
      (kbd "RET") nil
      (kbd "<return>") nil
      (kbd "C-h") nil)

    )

  )  ;; }}}

(progn  ;; Flycheck  {{{
  (use-package flycheck
    :custom
    (flycheck-python-pylint-executable "pylint")
    (flycheck-emacs-lisp-load-path 'inherit)
    (flycheck-disabled-checkers '(python-ruff))  ;; use lsp for ruff
    (flycheck-mode-line-color nil)
    :hook (prog-mode-local-only . flycheck-mode)
    :config

    ;; https://github.com/flycheck/flycheck/issues/1762
    (defvar-local my/lsp-next-checkers nil "Custom :next-checkers for lsp checker")
    (my/define-advice flycheck-checker-get (:around (fn checker property) lsp-next-checker)
      "Custom flycheck-checker-get to use my/lsp-next-checkers"
      (if (and (equal checker 'lsp) (equal property 'next-checkers))
          my/lsp-next-checkers
        (funcall fn checker property)))

    (evil-ex-define-cmd "cope[n]" #'flycheck-list-errors)
    (evil-define-key 'normal 'global
      (kbd "g ?") #'flycheck-display-error-at-point)
    ;; also see evil-collection for more keybindings

    ;; optimize flycheck-list-errors buffer
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.20)))

    ;; Language specific settings
    ;; https://github.com/flycheck/flycheck/issues/1475
    ;; otherwise, it runs eslint --print-config, which is slow
    (my/define-advice flycheck-eslint-config-exists-p (:override (fn) ignore)
      t))

  (use-package consult-flycheck
    :init (evil-define-key 'normal 'global
            (kbd "g !") #'consult-flycheck))

  (use-package flycheck-google-cpplint
    :after flycheck
    :custom (flycheck-c/c++-googlelint-executable "cpplint")
    :demand t)

  (use-package flycheck-package
    :after flycheck)

  ;; (use-package sideline
  ;;   :hook (flycheck-mode . sideline-mode)
  ;;   :init (setq sideline-backends-right '(sideline-flycheck)
  ;;               sideline-order-right 'down)
  ;;   NOTE: the inhibit-display-functions is not working
  ;;   :config (add-hook 'sideline-inhibit-display-functions #'evil-insert-state-p))

  ;; (use-package sideline-flycheck
  ;;   :hook (flycheck-mode . sideline-flycheck-setup)
  ;;   https://github.com/emacs-sideline/sideline-flycheck/issues/2
  ;;   :init (setq flycheck-display-errors-delay 0.3  ;; NOTE: sideline-delay
  ;;               sideline-flycheck-show-checker-name t))

  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config
    ;; https://github.com/alexmurray/flycheck-posframe/issues/25
    (setq flycheck-posframe-hide-posframe-hooks nil
          flycheck-posframe-timeout 0.0
          flycheck-display-errors-delay 0.2)

    (defun my/flycheck-posframe-inhibit ()
      "Return t if we should inhibit flycheck posframe."
      ;; only show in normal state
      (not (equal evil-state 'normal)))
    (add-hook 'flycheck-posframe-inhibit-functions
              #'my/flycheck-posframe-inhibit)

    (defun my/flycheck-posframe-hide-in-current-buffer ()
      "Hide flycheck posframe in current buffer if any."
      (let ((trigger-buf (current-buffer)))
        (posframe-funcall
         flycheck-posframe-buffer
         (lambda ()
           (let ((posframe-parent-buf (cdr (frame-parameter nil 'posframe-parent-buffer))))
             (when (eq trigger-buf posframe-parent-buf)
               (flycheck-posframe-hide-posframe)))))))
    ;; evil-normal-state-entry-hook: hide posframe on ESC in normal state
    (add-hook 'evil-normal-state-entry-hook #'my/flycheck-posframe-hide-in-current-buffer)

    (defun posframe-poshandler-my-point-window-right (info)
      (let ((res0 (posframe-poshandler-point-bottom-left-corner info))
            (res1 (posframe-poshandler-window-top-right-corner info)))
        (cons (- (car res1) (cadr (window-fringes)))
              (cdr res0))))
    (setq flycheck-posframe-position 'my-point-window-right
          posframe-mouse-banish nil
          flycheck-posframe-error-prefix "\u26d4 "
          flycheck-posframe-warning-prefix "\u26a0 ")
    :custom-face
    (flycheck-posframe-face ((t :height 0.9)))
    (flycheck-posframe-background-face ((t :inherit hl-line)))
    (flycheck-posframe-warning-face ((t :inherit warning :height 0.9)))
    (flycheck-posframe-error-face ((t :inherit error :height 0.9))))
  )  ;; }}}

(comment  ;; Flymake  {{{
  (use-package flymake-posframe
    :hook (flymake-mode . flymake-posframe-mode))
  )  ;; }}}

(progn  ;; LSP-mode  {{{
  (use-package lsp-mode
    :init
    (setenv "LSP_USE_PLISTS" "true")  ;; also set in Makefile, to be effective while byte compiling
    (setq
     lsp-keymap-prefix "C-S-l"
     lsp-clients-clangd-args '("--background-index=false" "--header-insertion-decorators" "--log=error")
     lsp-enable-snippet nil
     lsp-enable-on-type-formatting nil  ;; laggy
     lsp-enable-indentation nil  ;; disable lsp-format using evil "=". use "+" for lsp-format. see below
     lsp-enable-file-watchers t
     lsp-lens-enable nil
     lsp-idle-delay 0.5
     read-process-output-max (* 1024 1024)
     lsp-signature-auto-activate nil  ;; disable auto activate. use "C-l" to trigger
     lsp-prefer-capf t
     lsp-modeline-code-actions-enable nil
     lsp-auto-execute-action nil  ;; open selection menu even if there's one action
     ;; disable breadcrumb by default, enable by "prefix T b"
     lsp-headerline-breadcrumb-enable nil
     ;; disable "gray" font for unused variables
     lsp-diagnostics-attributes '()
     ;; we already have flycheck, no need for extra modeline diagnostics
     lsp-modeline-diagnostics-enable nil
     ;; we will configure company/corfu by ourselves
     lsp-completion-provider :none
     ;; imenu style
     lsp-imenu-index-function #'lsp-imenu-create-categorized-index)
    (evil-define-minor-mode-key 'normal 'lsp-mode
      (kbd "g r") #'lsp-find-references
      (kbd "g x") #'lsp-execute-code-action)
    (evil-define-minor-mode-key 'visual 'lsp-mode
      (kbd "+") #'lsp-format-region)
    (evil-define-minor-mode-key '(normal motion) 'lsp-mode
      (kbd "++") #'lsp-format-buffer)
    (evil-define-minor-mode-key 'insert 'lsp-mode
      (kbd "C-l") #'lsp-signature-activate)
    (defun my/maybe-start-lsp ()
      "Run `lsp-deferred' if the following condition matches:
1. major modes not blacklisted;
2. folder is already imported.
Otherwise, I should run `lsp' manually."
      (when (and (not (memq major-mode '(xonsh-mode bpftrace-mode)))
                 (buffer-file-name)
                 (lsp-find-session-folder (lsp-session) (buffer-file-name)))
        (lsp-deferred)))
    :hook ((c++-mode . my/maybe-start-lsp)
           (c++-ts-mode . my/maybe-start-lsp)
           (c-mode . my/maybe-start-lsp)
           (c-ts-mode . my/maybe-start-lsp)
           (objc-mode . my/maybe-start-lsp)
           (python-mode . my/maybe-start-lsp)
           (python-ts-mode . my/maybe-start-lsp)
           (go-mode . my/maybe-start-lsp)
           (go-ts-mode . my/maybe-start-lsp)
           (rust-mode . my/maybe-start-lsp)
           (rust-ts-mode . my/maybe-start-lsp)
           (haskell-mode . my/maybe-start-lsp)
           (haskell-literate-mode . my/maybe-start-lsp)
           (js-mode . my/maybe-start-lsp)
           (typescript-ts-base-mode . my/maybe-start-lsp)

           (lsp-mode . lsp-enable-which-key-integration))
    :commands (lsp lsp-deferred lsp-find-session-folder)
    :delight
    '(" #"
      (lsp--buffer-workspaces
       (:eval (mapconcat (lambda (w) (symbol-name (lsp--workspace-server-id w)))
                         lsp--buffer-workspaces "/"))
       (:propertize "?" face warning)))
    :config
    (evil-define-key nil lsp-signature-mode-map
      (kbd "C-n") #'lsp-signature-next
      (kbd "C-p") #'lsp-signature-previous
      (kbd "C-j") #'lsp-signature-next
      (kbd "C-k") #'lsp-signature-previous)
    ;; https://emacs-lsp.github.io/lsp-mode/page/faq/
    ;; forget the workspace folders for multi root servers so the workspace folders are added on demand
    (my/define-advice lsp (:before (&rest _) ignore-multi-root)
      "Ignore multi-root while starting lsp."
      (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))

    ;; shutdown workspaces (servers) without any buffers
    ;; those would be auto shutdown if `lsp-keep-workspace-alive' is nil, but I didn't set it
    ;; because in some cases it's expensive to start the workspace again
    (defun my/lsp-shutdown-idle-workspaces ()
      (interactive)
      (mapc (lambda (w)
              (when (not (lsp--workspace-buffers w))
                (message "Shutting down idle workspace %s..." (lsp--workspace-print w))
                (lsp-workspace-shutdown w)))
            (apply #'append
                   (hash-table-values (lsp-session-folder->servers (lsp-session))))))
    (evil-define-key 'normal 'global
      (kbd "C-S-l w Q") #'my/lsp-shutdown-idle-workspaces)

    (my/define-advice json-parse-buffer (:around (old-fn &rest args) lsp-booster-parse-bytecode)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))

    (my/define-advice lsp-resolve-final-command (:around (old-fn cmd &optional test?) add-lsp-server-booster)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))

    ;; this is mostly for bazel. to avoid jumping to bazel execroot
    (my/define-advice lsp--uri-to-path (:filter-return (path) follow-symlink)
      (file-truename path)))

  (use-package lsp-pyright
    :demand t
    :after lsp-mode
    :custom (lsp-pyright-multi-root nil))

  (use-package lsp-haskell
    :demand t
    :after lsp-mode)

  ;; using flycheck-posframe for flycheck error messages now
  ;; using lsp-modeline-code-actions-enable for code action now
  (comment lsp-ui
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-actions-icon nil)
    (evil-define-minor-mode-key 'normal 'lsp-ui-mode
      (kbd "g h") #'lsp-ui-doc-glance)
    :commands lsp-ui-mode  ;; will be called by lsp
    ;; display flycheck errors using sideline even for non-lsp buffers
    ;; follow https://github.com/emacs-lsp/lsp-ui/issues/437 for future compatibility
    :hook (flycheck-mode . lsp-ui-mode))
  )  ;; }}}

(comment  ;; Eglot  {{{
 ;; Why not eglot?
 ;; it's not good for many small things.
 ;; For example, lsp-mode would parse the symbol information returned by clangd,
 ;; and only display the required information in the minibuffer.
 ;; Eglot, however, would display the original content (very long paragraph)
  (use-package eglot
    :custom (eglot-autoshutdown t)
    ;; :hook ((c++-mode . eglot-ensure))
    :config (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd" "--background-index=false")))
  )  ;; }}}

(progn  ;; External integration {{{
  (use-package magit
    :init
    (evil-define-key 'normal 'global
      (kbd "C-s") 'magit
      (kbd "<C-m>") 'magit-file-dispatch
      (kbd "C-S-m") 'magit-dispatch)
    ;; Too slow in some projects
    ;; (setq magit-commit-show-diff nil)
    :commands magit
    :config
    ;; https://github.com/magit/magit/issues/4353
    (defun my/wrap-git-commit-setup-font-lock (orig-fn &rest args)
      "Wrapper function for git-commit-setup-font-lock, disable listing branch names, speed it up"
      (defun my/return-empty-cons (&rest args)
        "Simply return '()"
        '())
      (advice-add 'magit-list-local-branch-names :override #'my/return-empty-cons)
      (advice-add 'magit-list-remote-branch-names :override #'my/return-empty-cons)
      (let ((res (apply orig-fn args)))
        (advice-remove 'magit-list-local-branch-names #'my/return-empty-cons)
        (advice-remove 'magit-list-remote-branch-names #'my/return-empty-cons)
        res))
    (advice-add 'git-commit-setup-font-lock :around #'my/wrap-git-commit-setup-font-lock)

    (remove-hook 'magit-status-headers-hook #'magit-insert-upstream-branch-header)
    (remove-hook 'magit-status-headers-hook #'magit-insert-push-branch-header)
    (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header)

    (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-pushremote)
    (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent)
    (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-pushremote)
    (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-upstream)

    ;; (let ((gitconfig-fsmonitor (expand-file-name "~/.gitconfig_fsmonitor")))
    ;;   (when (file-exists-p gitconfig-fsmonitor)
    ;;     (setq magit-git-global-arguments (append `("-c" ,(concat "include.path=" gitconfig-fsmonitor))
    ;;                                              magit-git-global-arguments))))
    )

  (use-package pr-review
    :init
    (evil-ex-define-cmd "prr" #'pr-review)
    (evil-ex-define-cmd "prs" #'pr-review-search)
    (evil-ex-define-cmd "prn" #'pr-review-notification)
    (add-to-list 'browse-url-default-handlers
                 '(pr-review-url-parse . pr-review-open-url))
    :config
    (setq pr-review-generated-file-regexp
          (rx (or (: (*? anychar) "generated/" (* anychar))
                  (: (*? anychar) (or "pdm.lock" "pdm_lock.bzl") eol))))
    (defun my/enable-company-emoji-buffer-local ()
      (set (make-local-variable 'company-backends)
           '(company-emoji company-capf)))
    (add-hook 'pr-review-input-mode-hook #'my/enable-company-emoji-buffer-local))

  (use-package git-link
    :init
    (evil-define-key '(normal visual) 'global
      (kbd "C-c l") #'git-link-dispatch)
    :custom (git-link-default-branch "master"))

  (use-package rg
    :init
    (evil-define-key 'normal 'global
      (kbd "C-c s") #'rg-menu)
    (setq wgrep-auto-save-buffer t)
    :commands (my/rg-run-raw rg-menu)
    :config
    (defun my/rg-run-raw (args &optional dir)
      "Run raw rg command with ARGS (string) in DIR."
      (let* ((default-directory (or dir default-directory))
             (display-buffer-alist '((".*" display-buffer-same-window)))
             (cmd (concat rg-executable " "
                          (mapconcat #'identity rg-required-command-line-flags " ")
                          " --column --heading "
                          args))
             (search (rg-search-create :full-command cmd)))
        (with-current-buffer (compilation-start cmd 'rg-mode #'rg-buffer-name)
          (rg-mode-init search)))))

  (use-package fcitx
    :demand t
    :config
    (setq fcitx-use-dbus
          (cond ((my/macos-p) nil)
                ((dbus-ping :session "org.fcitx.Fcitx5") 'fcitx5)
                (t t)))
    (fcitx-evil-turn-on))

  (use-package xref  ;; builtin
    :config
    ;; never use etags in xref
    (remove-hook 'xref-backend-functions #'etags--xref-backend))

  (use-package dumb-jump
    :commands (my/xref-dumb-jump)
    :init
    (setq dumb-jump-selector 'completing-read
          dumb-jump-default-project "~/Code/"
          dumb-jump-force-searcher 'rg)
    (evil-define-key 'normal 'global
      (kbd "g]") #'my/xref-dumb-jump)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    :config
    (defun my/xref-dumb-jump ()
      (interactive)
      ;; only use dumb-jump; also prompt for identifier
      (let ((xref-backend-functions '(dumb-jump-xref-activate))
            (current-prefix-arg '(4)))
        (call-interactively #'xref-find-definitions)))

    (my/define-advice dumb-jump-go (:before (&rest _) set-jump)
      (evil-set-jump))
    (my/define-advice dumb-jump-get-project-root (:override (filepath) use-projectile)
      (s-chop-suffix "/" (expand-file-name
                          (or (projectile-project-root filepath)
                              dumb-jump-default-project)))))

  (use-package sudo-edit
    :init (evil-ex-define-cmd "su[do]" #'sudo-edit)
    :commands (my/find-file-fallback-sudo)
    :config
    (sudo-edit-indicator-mode)

    (defun my/find-file-fallback-sudo (file)
      "Like `find-file', but falls back to `sudo-edit' if file cannot be opened."
      (condition-case err
          (find-file file)
        (file-error (when (y-or-n-p (format "Find file error: %s. Try open as sudo?" (cdr err)))
                      (sudo-edit-find-file file))))))

  (use-package server
    :demand t
    :config
    ;; Make sure the server is running.
    ;; (copied from with-editor)
    (unless (process-live-p server-process)
      (when (server-running-p server-name)
        (setq server-name (format "server%s" (emacs-pid)))
        (when (server-running-p server-name)
          (server-force-delete server-name)))
      (server-start))
    ;; set window property for navigate-emacs.bash
    (when (eq window-system 'x)
      (when (fboundp 'x-change-window-property)
        (x-change-window-property "EMACS_SERVER_NAME" server-name (selected-frame) nil nil t nil))
      (setq frame-title-format '("Emacs:SERVER_NAME=" server-name))))

  (use-package man
    :custom (Man-notify-method 'pushy)
    :commands man)

  (use-package woman
    :custom (woman-fill-frame t)
    :commands woman-find-file-with-fallback
    :init (evil-define-key '(normal motion) 'global
            (kbd "C-h M") #'woman)
    :config
    (defun woman-find-file-with-fallback (path)
      "Find file with woman, fallback to man on error."
      (condition-case-unless-debug nil
          (woman-find-file path)
        (error
         (message "Failed to open %s using woman, falling back to man..." path)
         (man (concat "-l " (shell-quote-argument path)))))))

  (use-package eww
    :init
    (evil-ex-define-cmd "go[ogle]" #'eww)
    :config
    ;; google would return a simplified version for "w3m"
    (setq url-user-agent (format "Emacs/%s (%s); like w3m/0.5.0" emacs-version system-type)
          eww-search-prefix "https://www.google.com/search?gl=us&hl=en&q="
          eww-auto-rename-buffer 'title)

    ;; Make the toggles affect current buffer only
    (my/define-advice eww-toggle-colors (:before (&rest _) make-variable-buffer-local)
      (make-local-variable 'shr-use-colors))
    (my/define-advice eww-toggle-images (:before (&rest _) make-variable-buffer-local)
      (make-local-variable 'shr-inhibit-images))
    (my/define-advice eww-toggle-fonts (:before (&rest _) make-variable-buffer-local)
      (make-local-variable 'shr-use-fonts))

    (define-key eww-link-keymap "w" nil)
    (evil-define-key 'normal eww-mode-map
      (kbd "C-o") #'eww-back-url
      (kbd "zf") #'eww-toggle-fonts
      (kbd "zc") #'eww-toggle-colors
      (kbd "zi") #'eww-toggle-images
      ;; recover some evil keybindings. they are set to ignore in special-mode-map
      [remap evil-insert] #'evil-insert)

    (defun my/remove-google-url-redirect (link)
      "Remove google url redirect."
      (if-let* ((url (url-generic-parse-url link))
                ((and (string-match-p (rx (or "." bos) "google.com") (url-host url))
                      (string-match-p (rx bos "/url?") (url-filename url))))
                (query (url-parse-query-string (replace-regexp-in-string (rx bos "/url?") "" (url-filename url))))
                (target-pair (or (assoc "url" query)
                                 (assoc "q" query)
                                 (assoc "imgurl" query)))
                (target (car (cdr target-pair))))
          (url-unhex-string target)
        link))
    (add-to-list 'eww-url-transformers #'my/remove-google-url-redirect))

  (use-package browse-url
    :init (evil-define-key '(normal motion) 'global
            (kbd "g l") #'browse-url
            (kbd "g L") #'browse-url-default-browser))

  (use-package devdocs-browser
    :custom
    (devdocs-browser-enable-cache nil)
    (devdocs-browser-open-fallback-to-all-docs nil)
    :init
    (evil-define-key nil 'global
      (kbd "C-h d") #'devdocs-browser-open
      (kbd "C-h D") #'devdocs-browser-open-in)
    ;; https://github.com/emacs-evil/evil/issues/301
    (evil-define-minor-mode-key 'normal 'devdocs-browser-eww-mode
      (kbd "g o") #'devdocs-browser-eww-open-in-default-browser))

  (use-package suggest)

  (comment webkit
    :init (require 'ol)
    :config
    (my/define-advice webkit-rename-buffer (:filter-args (args) rename-buffer)
      (let ((title (car args)))
        (list (if (string= "" title)
                  title
                (concat "*Webkit* " title)))))
    (require 'webkit-ace)
    (require 'evil-collection-webkit)
    (evil-collection-xwidget-setup)

    (modify-all-frames-parameters '((inhibit-double-buffering . t))))

  (use-package pydoc)

  (use-package auth-source
    :custom
    (auth-source-pass-filename "~/.password-store/emacs/")
    :config
    (auth-source-pass-enable))

  )  ;; }}}

(progn  ;; AI {{{

  (use-package gptel
    :init
    (evil-define-key '(normal visual) 'global
      (kbd "C-c a") #'gptel-send)
    (evil-ex-define-cmd "ai" #'gptel)
    :config
    (evil-define-minor-mode-key 'normal 'gptel-mode
      (kbd "C-c C-k") #'gptel-abort)

    (setq gptel-default-mode 'markdown-mode)

    (defun my/gptel-buffer-setup ()
      "Setup `gptel-mode' buffer settings."
      (setq-local truncate-lines nil)
      (evil-define-key nil gptel-mode-map
        (kbd "C-c C-c") #'gptel-send))
    (add-hook 'gptel-mode-hook #'my/gptel-buffer-setup)

    (my/define-advice gptel-send (:before (&rest _) goto-eob)
      "Goto end of buffer before sending while in `gptel-mode'."
      (when gptel-mode
        (goto-char (point-max)))))

  (use-package codeium
    :commands (my/codeium-begin)
    :init
    ;; only complete when specifically triggered
    (evil-define-key 'insert prog-mode-map
      (kbd "C-f") #'my/codeium-begin)

    :config
    (defvar-local my/codeium-company-frontends-backup nil)
    (defun my/codeium-recover-company-frontends (&rest _)
      (when my/codeium-company-frontends-backup
        (setq-local company-frontends my/codeium-company-frontends-backup
                    my/codeium-company-frontends-backup nil)))
    (add-hook 'company-after-completion-hook #'my/codeium-recover-company-frontends)

    (defun my/codeium-begin ()
      (interactive)
      ;; temporary set company-frontends to preview
      (unless my/codeium-company-frontends-backup
        (setq-local my/codeium-company-frontends-backup company-frontends
                    company-frontends '(company-preview-frontend)))
      (let ((completion-at-point-functions '(codeium-completion-at-point)))
        (call-interactively #'company-capf)))

    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t))

  (comment copilot
    :hook (prog-mode . copilot-mode)
    :delight " \xe70a"  ;; 
    :init
    (setq copilot-idle-delay 0.2
          copilot-log-max 0)  ;; during profiling, we can see that the event logging takes ~50% time
    ;; autobalancer seems buggy for now
    (setq copilot-balancer-lisp-modes nil)
    (evil-define-key 'insert 'global
      (kbd "C-f") #'my/copilot-complete-or-accept)
    :commands (my/copilot-complete-or-accept)
    :config
    (require 'warnings)
    (add-to-list 'warning-suppress-types '(copilot))

    (defvar-local my/copilot-inhibited nil)
    (defun my/copilot-inhibited-p ()
      (or my/copilot-inhibited
          completion-in-region-mode  ;; corfu
          (bound-and-true-p company-backend)  ;; this is true when backend is active (even when there are no candidates)
          (and (fboundp 'company--active-p) (company--active-p))  ;; this is true when there are candidates (frontend is alive)
          ))

    (with-eval-after-load 'company
      (defun my/copilot-hide-company-frontend (action)
        "A fake company frontend, used to hide copilot.
So that copilot and company mode will not affect each other."
        (pcase action
          ;; 'update 'pre-command
          ((or 'show 'update)
           (setq-local my/copilot-inhibited t)
           (when copilot-mode
             (copilot-clear-overlay)))
          ('hide
           (setq-local my/copilot-inhibited nil))))
      (add-to-list 'company-frontends #'my/copilot-hide-company-frontend)  ;; add to front
      )

    (defun my/copilot-maybe-hide ()
      "Hide copilot when completion-in-region-mode is active."
      (when (and copilot-mode completion-in-region-mode)
        (copilot-clear-overlay)))
    (add-hook 'completion-in-region-mode-hook #'my/copilot-maybe-hide)

    (defun my/copilot-complete-or-accept ()
      "Complete or accept completion."
      (interactive)
      ;; abort company if active
      (when (and (fboundp 'company-abort) company-mode)
        (company-abort))
      (when (and (fboundp 'corfu-quit) corfu-mode)
        (corfu-quit))
      (if (copilot-current-completion)
          (copilot-accept-completion)
        (copilot-complete)))

    (evil-define-key nil copilot-completion-map
      (kbd "C-S-f") #'copilot-accept-completion-by-line
      (kbd "C-j") #'copilot-next-completion
      (kbd "C-k") #'copilot-previous-completion
      (kbd "C-l") #'copilot-clear-overlay)

    ;; only inhibit copilot display, not completion triggering, to reduce the latency
    ;; (add-to-list 'copilot-disable-predicates #'my/copilot-inhibited-p)
    (add-to-list 'copilot-disable-display-predicates #'my/copilot-inhibited-p))

  ) ;; }}}

(progn  ;; Email  {{{
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user "i@blahgeek.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        user-mail-address "yikai@z1k.dev"
        user-full-name "Yikai Zhao")
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port "imaps")
                 (nnimap-user "i@blahgeek.com")
                 (nnimap-stream ssl)))

  (use-package notmuch
    :init
    (evil-ex-define-cmd "nm" #'notmuch)
    :custom
    (notmuch-search-oldest-first nil)
    (notmuch-show-logo nil)
    (notmuch-archive-tags '("-inbox" "-unread"))
    ;; Move authors field to the end, and set its height to x0.8, to workaround chinese font alignment issue (chinese name in authors)
    (notmuch-search-result-format '(("date" . "%12s ")
                                    ("count" . "%-7s ")
                                    ("subject" . "%s ")
                                    ("tags" . "(%s) ")
                                    ("authors" . "-- %-30s ")))
    :commands notmuch
    :custom-face
    (notmuch-search-matching-authors ((t (:height 0.75))))
    (notmuch-search-non-matching-authors ((t (:height 0.75))))
    :config
    (add-to-list 'notmuch-tag-formats
                 '("gh-.*" (propertize tag 'face 'notmuch-tag-unread)))
    ;; display text/html for github notifications
    (defun my/notmuch-multipart/alternative-discouraged (msg)
      (if (string-match-p "@github" (plist-get msg :id))  ;; match both @github.com and @github.corp.pony.ai
          '("text/plain")
        '("text/html" "multipart/related")))
    (setq notmuch-multipart/alternative-discouraged #'my/notmuch-multipart/alternative-discouraged)

    ;; display images
    (setq notmuch-show-text/html-blocked-images nil)

    ;; shr-tag-img will ignore images with size=1
    (my/define-advice shr-tag-img (:before (dom &rest _) fix-github-email-beacon-img)
      (when (string-match-p "/notifications/beacon/" (dom-attr dom 'src))
        (dom-set-attribute dom 'width "2")
        (dom-set-attribute dom 'height "2")))

    (defun my/notmuch-search-mark-read ()
      (interactive)
      (notmuch-search-tag '("-unread")))

    (defun my/notmuch-show-mark-read ()
      (interactive)
      (notmuch-show-tag-all '("-unread")))

    (defun my/notmuch-show-mark-unread ()
      (interactive)
      (notmuch-show-tag-all '("+unread")))

    (evil-define-key '(normal motion) notmuch-search-mode-map
      (kbd "R") #'my/notmuch-search-mark-read)

    (evil-define-key '(normal motion) notmuch-show-mode-map
      (kbd "R") #'my/notmuch-show-mark-read
      (kbd "U") #'my/notmuch-show-mark-unread)

    (defun my/notmuch-show-expand-unread-only ()
      (interactive)
      (let (any-unread)
        (notmuch-show-mapc
         (lambda () (when (member "unread" (notmuch-show-get-tags))
                      (setq any-unread t))))
        (when any-unread
          (notmuch-show-open-or-close-all)
          (notmuch-show-mapc
           (lambda () (unless (member "unread" (notmuch-show-get-tags))
                        (notmuch-show-toggle-message)))))))
    (add-hook 'notmuch-show-hook #'my/notmuch-show-expand-unread-only)
    )

  )  ;; }}}

(progn  ;; UI {{{
  (use-package pixel-scroll
    :demand t
    :when (>= emacs-major-version 29)
    :custom (pixel-scroll-precision-mode t)
    :config
    (define-key pixel-scroll-precision-mode-map [next] nil)
    (define-key pixel-scroll-precision-mode-map [prior] nil))
  )  ;; }}}

(progn  ;; Profiler {{{
  (use-package profiler  ;; builtin
    :init (setq profiler-max-stack-depth 64)
    :config
    (defun my/profiler-report-flamegraph--entry-name (entry)
      (string-replace
       " " "_"
       (cond
        ((eq entry t)
         "Others")
        ((and (symbolp entry) (fboundp entry))
         (symbol-name entry))
        (t
         (profiler-format-entry entry)))))

    (require 'f)
    (defun my/profiler-report-flamegraph ()
      (interactive)
      (let* ((tmpdir (make-temp-file "emacs-profiler-report-flamegraph-" 'dir))
             (txtfile (f-join tmpdir "flamegraph.txt"))
             (svgfile (f-join tmpdir "flamegraph.svg")))
        (with-temp-file txtfile
          (maphash
           (lambda (stacktrace count)
             (setq stacktrace (seq-reverse (seq-take-while #'identity stacktrace)))
             (if (length> stacktrace 0)
                 (insert (mapconcat #'my/profiler-report-flamegraph--entry-name stacktrace ";"))
               (insert "??"))
             (insert (format " %d\n" count)))
           profiler-cpu-log))
        (call-process "flamegraph.pl" txtfile `((:file ,svgfile) nil) nil)
        (browse-url-firefox svgfile))))
  )  ;; }}}

(progn  ;; Misc {{{
  (custom-set-variables
   '(auth-source-save-behavior nil)
   '(term-buffer-maximum-size 20480)

   ;; it was reversed... (wtf?)
   ;; https://mail.gnu.org/archive/html/emacs-devel/2019-03/msg00002.html
   '(tabulated-list-gui-sort-indicator-asc ?▲)
   '(tabulated-list-gui-sort-indicator-desc ?▼)

   ;; System UI related
   '(use-dialog-box nil)
   '(use-system-tooltips nil)
   '(x-gtk-use-system-tooltips nil))

  (setq ring-bell-function 'ignore)

  (use-package nsm
    :config
    ;; nsm-should-check would call `network-lookup-address-info',
    ;; which calls getaddrinfo, which is a blocking call...
    ;; it happens when using notmuch on a slow connection, emacs would block for a long time.
    ;; this function is useless anyway.
    (my/define-advice nsm-should-check (:override (&rest _) skip)
      t))

  ;; always cancel session shutdown, prevent writing session files
  (add-hook 'emacs-save-session-functions #'always)

  )  ;;; }}}

(progn  ;; Load custom.el, enable customization UI  {{{
  ;; set custom-file to another file, but only load SOME of them
  ;; steps to change variable using Customization UI: apply and save, review it, put it in this file.
  (setq custom-file "~/.emacs.d/custom.el")

  ;; Load some whitelisted variables from custom.el
  (setq my/allowed-custom-variables
        '(safe-local-variable-values custom-safe-themes codeium/metadata/api_key))

  (when-let ((_ (file-exists-p custom-file))
             (content (with-temp-buffer
                        (insert-file-contents-literally custom-file)
                        (goto-char (point-min))
                        (read (current-buffer))))
             ;; content => (custom-set-variables (quote (k1 v2)) (quote (k2 v2)))
             (is-custom-variable (eq (car content) 'custom-set-variables))
             (filtered-variables (seq-filter
                                  (lambda (e) (memq (car (cadr e)) my/allowed-custom-variables))
                                  (cdr content))))
    (apply 'custom-set-variables (mapcar 'cadr filtered-variables)))
  )  ;; }}}

;;; init.el ends here
