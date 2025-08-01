;;; init --- My config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq
 native-comp-async-report-warnings-errors nil
 garbage-collection-messages nil)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/")

(progn  ;; exec-path, PATH and other env
  (defun my/prepend-exec-path (p)
    (let ((path (expand-file-name p)))
      (when (file-directory-p path)
        (setq exec-path (cons path (delete path exec-path))))
      (setenv "PATH" (string-join exec-path ":"))))

  (my/prepend-exec-path "/opt/local/bin")
  (my/prepend-exec-path "/opt/local/sbin")
  (my/prepend-exec-path "/usr/local/bin")
  (my/prepend-exec-path "/usr/local/sbin")
  (my/prepend-exec-path "~/.npm/bin")
  (my/prepend-exec-path "~/go/bin")
  (my/prepend-exec-path "~/.npm-packages/bin")
  (my/prepend-exec-path "~/.cargo/bin")
  (my/prepend-exec-path "~/.rvm/bin")
  (my/prepend-exec-path "~/.local/bin")
  (my/prepend-exec-path (file-name-concat user-emacs-directory "bin"))

  ;; handle all dotfiles in .emacs.d
  (let ((emacs-dir (expand-file-name user-emacs-directory)))
    (setenv "XONSHRC" (concat (file-name-concat emacs-dir "xonsh_rc.xsh")
                              ":~/.xonshrc"))
    (setenv "XONSH_CONFIG_DIR" emacs-dir)
    ;; set here for both xonsh and magit
    (setenv "GIT_CONFIG_GLOBAL" (file-name-concat emacs-dir "dotfiles/git/config"))
    (setenv "RIPGREP_CONFIG_PATH" (file-name-concat emacs-dir "dotfiles/ripgrep.config"))
    (setenv "NOTMUCH_CONFIG" (file-name-concat emacs-dir "dotfiles/notmuch/config"))

    (setq treesit-extra-load-path (list (file-name-concat emacs-dir "treesit-langs/dist/")))
    ))

;; some variables from init-local.el
(defvar my/curl-proxy nil)

(let ((my/-init-local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p my/-init-local-file)
    (load-file my/-init-local-file)))

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
  (setq use-package-verbose nil
        ;; always defer. this is important
        use-package-always-defer t)

  (require 'use-package)

  (progn
    ;; my/env-check :
    ;;  like health check, used for check if os environment meets package requirement.
    ;;  add checks using :my/env-check in package configurations.

    (defvar my/env-check-functions nil)
    (defun my/env-check ()
      (interactive)
      (with-current-buffer-window "*my/env-check*" nil nil
          (erase-buffer)
          (run-hooks 'my/env-check-functions)))

    (add-to-list 'use-package-keywords :my/env-check)
    (defalias 'use-package-normalize/:my/env-check 'use-package-normalize-forms)
    (defun use-package-handler/:my/env-check (name _keyword args rest state)
      (use-package-concat
       (when args
         (let ((fn-name (intern (format "my/env-check-package--%s" name))))
           `((defun ,fn-name ()
               (insert ,(format "Checking for `%s' (%d conditions)...\n" name (length args)))
               (require ',name)
               ,@(mapcar (lambda (form)
                           `(unless ,form
                              (insert (propertize "  Failed" 'face 'error) ,(format ": %s\n" form))))
                         args))
             (add-to-list 'my/env-check-functions ',fn-name))))
       (use-package-process-keywords name rest state))))

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
  ) ;; }}}

(progn  ;; Some utility helper functions {{{
  (defun my/macos-p ()
    "Return t if it's in macos."
    (string-equal system-type "darwin"))

  (defconst my/in-kitty (equal (getenv-internal "TERM" initial-environment) "xterm-kitty"))

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

(progn  ;; startup
  (setq inhibit-startup-echo-area-message t
        initial-major-mode 'fundamental-mode)

  (defvar my/startup-msg
    (if window-system
        "Welcome back   丨 🏴‍☠️\n\n"   ;; test various font and icon
      ;; for some unknown reason, the above chars would break terminal display. FIXME
      "Welcome back \n\n"))

  (defun my/startup-buffer ()
    (let ((default-directory "~/"))
      (with-current-buffer (get-buffer-create "*Welcome*")
        (insert my/startup-msg
                (format "native-comp-available-p: %s\n" (native-comp-available-p))
                (format "treesit-available-p: %s\n" (treesit-available-p))
                "\n"
                (format "Startup time: %s\n\n" (emacs-init-time))
                (string-replace "\n" "" (emacs-version)) "\n\n")
        (current-buffer))))
  (setq initial-buffer-choice #'my/startup-buffer)
  )

(when window-system  ;; delight icons, ligatures, fonts {{{
  (use-package ligature
    :config
    (ligature-set-ligatures
     'prog-mode '("!!" "!=" "!!!" "!==" "&&" "***" "*=" "*/" "++" "+=" "--" "-="
                  "->" ".." "..." "/*" "//" "/>" "///" "/**" ":::" "::" ":=" "<-"
                  "<<" "<=" "<=>" "==" "===" "=>" ">=" ">>" "??" "\\\\" "|=" "||"
                  "[[" "]]"))
    :hook (prog-mode . ligature-mode))

  ;; pragmata major mode icons
  ;; force using PragmataPro font (instead of the "Mono" one, whose icon is smaller)
  (let* ((icon-face '(:family "PragmataPro"))
         (plus-ts-suffix (propertize "+\xe21c" 'face icon-face))
         delight-args)
    (dolist (pair `((dired-mode . "\xf4d3")
                    (wdired-mode . ,(propertize "\xf4d3" 'face 'error))
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
                    (eat-mode . "\xe795")
                    (dockerfile-mode . "\xe7b0")))
      (add-face-text-property 0 (length (cdr pair)) icon-face nil (cdr pair))
      (push (list (car pair) (cdr pair) :major)
            delight-args)
      (let ((ts-mode (intern (string-replace "-mode" "-ts-mode" (symbol-name (car pair))))))
        (when (fboundp ts-mode)
          (push (list ts-mode (concat (cdr pair) plus-ts-suffix) :major) delight-args))))
    (delight delight-args))

  ;; see delight.el
  (with-eval-after-load 'cc-mode
    (my/define-advice c-update-modeline (:override (&rest _) ignore-for-delight)
      nil))

  ;; default font is set in early-init.el for fast startup

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
  (set-fontset-font t #x2026 "PragmataPro Mono Liga")

  (setq my/cn-font-name (if (my/macos-p) "HYQiHeiY1" "HYQiHeiY1-55W"))
  ;; 汉仪旗黑Y1（如果需要更扁，还可以选择Y2等）和英文等宽(x2)且等高
  (dolist (range '((#x2e80 . #x9fff)  ;; https://unicodeplus.com/plane/0
                   (#xf900 . #xfaff)
                   (#xfe30 . #xfe4f)
                   (#xff00 . #xffef)))
    ;; 它的不同weight是放在不同的字体里的，所以显式地选择55W作为regular
    ;; 需要加粗时，emacs会自动基于这个字体动态加粗
    (set-fontset-font t range (font-spec :family my/cn-font-name)))
  ;; twitter emoji font 等宽且等高 |🐶|
  (set-fontset-font t 'emoji "Twitter Color Emoji")

  ;; TODO: also set different chinese font for variable-pitch
  ;; to do that, we need to define our own fontset
  ;; https://www.reddit.com/r/emacs/comments/mo0cc8/whats_the_relation_between_setfontsetfont_and/
  (unless (my/macos-p)
    (set-face-attribute 'variable-pitch nil
                        :family "Noto Sans"))

  (defun my/check-all-fonts-exists ()
    (let ((allfonts (font-family-list)))
      (dolist (font (list (face-attribute 'default :family)
                           "PragmataPro Mono Liga"
                           "Twitter Color Emoji"
                           my/cn-font-name
                           "Noto Sans"))
        (insert (format "Checking for font `%s'...\n" font))
        (unless (member font allfonts)
          (insert (propertize "  Not found\n" 'face 'error))))))
  (add-to-list 'my/env-check-functions #'my/check-all-fonts-exists)

  )  ;; }}}

(progn  ;; EVIL & general keybindings {{{
  (when window-system
    ;; https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
    ;; to define C-m key
    (define-key input-decode-map [?\C-m] [C-m])
    ;; remove some keybindings that can be accidentally triggered..
    ;; suspend-frame
    (define-key global-map (kbd "C-x C-z") nil)
    ;; save-buffers-kill-terminal
    (define-key global-map (kbd "C-x C-c") nil))

  (use-package kkp
    :when my/in-kitty
    :demand t
    :commands (my/kkp-switch-layout)
    :config
    (defun my/kkp-switch-layout (layout)
      (interactive (list (completing-read "Layout: " '(linux-mod macos-mod none) nil t)))
      ;; my expected layout from left to right: alt, win (super), control
      (pcase layout
        ;; pc layout: control, win (super), alt.
        ;; aka, switch control and alt
        ('linux-mod (setq kkp-control-modifier 'meta
                          kkp-super-modifier 'super
                          kkp-alt-modifier 'control))
        ;; macos layout: control, opt (as alt in kitty), cmd.
        ('macos-mod (setq kkp-control-modifier 'meta
                          kkp-alt-modifier 'super
                          kkp-super-modifier 'control))
        ('none (setq kkp-control-modifier 'control
                     kkp-super-modifier 'super
                     kkp-alt-modifier 'meta))))
    (my/kkp-switch-layout 'macos-mod)

    (setq my/startup-msg
          (concat my/startup-msg
                  "Kitty terminal detected. See :kkp-status.\n"
                  "Using macos-mod layout. Use :my/kkp-switch-layout to switch.\n"
                  "\n"))

    (global-kkp-mode t))

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
    (setq evil--jumps-buffer-targets
          "\\`\\*\\(Man\\|new\\|scratch\\)")
    :config
    (evil-mode t)
    ;; Make :x :q :wq close buffer instead of closing window
    (evil-define-command evil-quit (&optional force)
      "Kill current buffer."
      :repeat nil
      (interactive "<!>")
      (kill-current-buffer))
    (evil-ex-define-cmd "bd[elete]" #'kill-current-buffer)
    ;; motion is also used by normal and visual
    (evil-define-key 'motion 'global
      ;; mouse:
      ;; 1. disable drag to visual mode
      [down-mouse-1] nil
      ;; nnoremap j gj
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line)
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
      (kbd "U") #'evil-redo))

  (progn  ;; movement
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
      (kbd "C-w C-l") #'my/move-buffer-to-window-right)

    ;; only used as fallback when the desktop environment does not have these bindings
    (evil-define-key nil 'global
      (kbd "s-h") #'evil-window-left
      (kbd "s-j") #'evil-window-down
      (kbd "s-k") #'evil-window-up
      (kbd "s-l") #'evil-window-right))

  (use-package evil-collection
    :demand t
    :after evil
    :custom
    (evil-collection-key-blacklist
     '(
       ;; evil-collection-pdf binds keys starting with /.  who would wants that???
       "//" "/!" "/="
       "/a" "/b" "/c" "/d" "/e" "/f" "/g" "/h" "/i" "/j" "/k" "/l" "/m" "/n" "/o" "/p" "/q" "/r" "/s" "/t" "/u" "/v" "/w" "/x" "/y" "/z"
       "/A" "/B" "/C" "/D" "/E" "/F" "/G" "/H" "/I" "/J" "/K" "/L" "/M" "/N" "/O" "/P" "/Q" "/R" "/S" "/T" "/U" "/V" "/W" "/X" "/Y" "/Z"
       ;; some of my own global keybindings
       "C-t" "C-r"
       ))
    (evil-collection-want-unimpaired-p nil)
    :config
    ;; remove keybindings for some modes. let's do them on our own
    (mapc (lambda (x) (setq evil-collection-mode-list (delete x evil-collection-mode-list)))
          '(vterm eat company corfu wdired go-mode))
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

  (use-package expand-region
    :commands (er/expand-region er/contract-region)
    :init (evil-define-key 'visual 'global
            (kbd ".") 'er/expand-region
            (kbd "v") 'er/expand-region
            (kbd ",") 'er/contract-region))

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
      ;; (kbd "s-Q") #'save-buffers-kill-emacs
      (kbd "s-f") #'toggle-frame-fullscreen))

  (use-package evil-terminal-cursor-changer
    :demand t
    :unless (display-graphic-p)
    :init
    (when my/in-kitty
      (setq etcc-term-type-override 'kitty))
    :config
    (etcc-on))

  ) ;; }}}

(use-package hydra  ;;; Hydra keybindings {{{
  :commands (my/hydra-copy-filename/body)
  :init
  (evil-define-key 'normal 'global
    (kbd "C-c y") #'my/hydra-copy-filename/body
    (kbd "C-c n") #'my/hydra-notes/body
    (kbd "C-n") #'my/hydra-notes/body)
  :config
  (defmacro my/define-hydra-copy-filename (args)
    "ARGS: list of (key . form)"
    (macroexpand
     `(defhydra my/hydra-copy-filename
        (:exit t :hint nil :color blue)
        ,(concat
          "
Copy filename as...
-------------------
"
          (mapconcat (lambda (arg)
                       (concat "_" (car arg) "_: %s" (prin1-to-string (cdr arg)) ""))
                     args "\n"))
        ,@(mapcar
           (lambda (arg) `(,(car arg) (kill-new ,(cdr arg))))
           args))))
  (my/define-hydra-copy-filename
   ;; project path
   (("y" . (let ((root (my/current-project-root)))
             (if root
                 (file-relative-name (buffer-file-name) root)
               (buffer-file-name))))
    ;; bazel path
    ("b" . (when-let* ((root (my/current-project-root))
                       (rel (file-relative-name (buffer-file-name) root)))
             (concat "//" (string-trim-right (or (file-name-directory rel) "") "/+") ":" (file-name-base rel))))
    ;; full path
    ("f" . (expand-file-name (buffer-file-name)))
    ;; short path
    ("s" . (file-relative-name (buffer-file-name)))))

  ;; note taking
  (defhydra my/hydra-notes
    (nil nil :exit t :color blue :hint nil)
    "
NOTES
=====

_s_: Search               _n_: New Scratch
_f_: Find file            ^ ^
_l_: Dired                ^ ^
"
    ("n" my/new-scratch-buffer)
    ("s" my/notes-search)
    ("f" my/notes-find-file)
    ("l" my/notes-dired))

  ) ;;; }}}

(progn  ;; Some essential utils {{{
  (use-package switch-buffer-functions
    :demand t)
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
      (when-let* ((pred (nth 1 args)))
        (when (functionp pred)
          (let ((new-pred (lambda (x) (and (funcall pred x) (not (string= x ""))))))
            (setf (nth 1 args) new-pred))))
      args))

  (use-package info
    :config
    (evil-define-key 'normal Info-mode-map
      (kbd "C-t") nil))

  (use-package term/xterm
    :when my/in-kitty
    ;; getSelection does not seem to work??
    :custom (xterm-extra-capabilities '(setSelection))
    :demand t
    :config (terminal-init-xterm))

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

  (defun my/patch-term-color-black (_)
    ;; Fix term-color-black:
    ;; By default, term-color-black is base02 (see solarized-faces.el),
    ;; which is, by its definition, a background color (very light in solarized-light).
    ;; However, shells don't need it as background, but instead they would use it to render text:
    ;; e.g. in xonsh, this color is used for displaying aborted commands and suggestions,
    ;; which should be a "comment"-like foreground color, which is base01
    (let ((color (face-foreground 'shadow)))
      (custom-set-faces
       `(term-color-black ((t (:foreground ,color :background ,color)))))))
  (add-hook 'enable-theme-functions #'my/patch-term-color-black)

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

  (setq frame-title-format "Emacs Makes A Computer Supreme")
  (setq-default mode-line-format
                (delete '(vc-mode vc-mode) mode-line-format))
  (custom-set-faces
   '(fixed-pitch ((t (:family nil :inherit default))))
   '(fixed-pitch-serif ((t :family nil :slant italic :inherit default)))
   '(line-number ((t (:height 0.9))))  ;; for pragmata, there's no light weight, let's use a smaller size
   '(mode-line ((t (:height 0.9))))  ;; smaller mode-line
   '(mode-line-inactive ((t (:height 0.9))))
   )

  ;; Font size management
  (defun my/change-font-size ()
    "Change font size based on predefined list"
    (interactive)
    (when-let* ((size-str (completing-read
                           "Select font size:"
                           (mapcar #'number-to-string my/gui-font-size-choices)))
                 (size-val (string-to-number size-str)))
      (when (> size-val 0)
        (my/gui-font-size-set size-val))))
  ;; NOTE: there's no way to implement auto-changing function
  ;; because my external monitor shares the same resolution with my laptop monitor
  (evil-define-key nil 'global
    (kbd "C-x =") #'my/change-font-size)

  ;; (add-to-list 'face-font-rescale-alist '(".*CJK.*" . 0.75))
  )  ;; }}}

(progn  ;; workspace management {{{

  (setq my/persp-profiles
        ;; the nil entry should contain all variables below and act as fallback values.
        ;; this is required because that, when switching profiles, some variables (belonging to some other packages)
        ;; may not be defined yet, so we would always need default values.
        `((nil . ((url-proxy-services . nil)
                  (pr-review-ghub-username . nil)
                  (pr-review-ghub-host . nil)))
          ("pony@home" . ((url-proxy-services . (("http" . "192.168.0.139:9877")
                                                 ("https" . "192.168.0.139:9877")))
                          (pr-review-ghub-username . "z1k-pony")
                          (pr-review-ghub-host . "github.corp.pony.ai/api/v3")))
          ))

  ;; simulate i3-like numbered workspace using perspective.el
  (use-package perspective
    :demand t
    :custom
    (persp-mode-prefix-key (kbd "C-c C-p"))
    (persp-initial-frame-name "0")
    (persp-show-modestring nil)
    (tab-bar-new-tab-choice 'clone)  ;; we create perspective AFTER creating tab, so the new-tab itself should not change window layout
    :config
    (defun my/scratch-buffer-p (buf)
      (let ((buf-name (buffer-file-name buf)))
        (and buf-name
             (string-match-p "Notes/scratch/scratch-.*\\.md$" buf-name))))

    (defun my/new-scratch-buffer ()
      (interactive)
      (let* ((date-str (format-time-string "%Y%m%d"))
             (random-str (substring (md5 (format "%s%s" (current-time) (random))) 0 5))
             (filename (expand-file-name
                        (format "scratch/scratch-%s-%s.md" date-str random-str)
                        "~/Notes")))
        (find-file filename)))

    (defun my/new-scratch-buffer-on-new-persp ()
      (my/new-scratch-buffer)
      (let ((old-scratch-name (persp-scratch-buffer)))
        (when (get-buffer old-scratch-name)
          (kill-buffer old-scratch-name))))
    (add-hook 'persp-created-hook #'my/new-scratch-buffer-on-new-persp)

    (persp-mode)
    (tab-bar-mode)

    (defun my/persp-use-profile (profile-name)
      (interactive (list (completing-read "Profile (empty to reset): "
                                          (mapcar #'car (cdr my/persp-profiles)))))
      (unless persp-mode
        (error "Only works when persp-mode is enabled"))
      (when (equal (persp-current-name) persp-frame-global-perspective-name)
        (error "Cannot apply profile to global perspective"))
      (let* ((kvs (alist-get profile-name my/persp-profiles nil nil 'equal))
             (default-kvs (alist-get nil my/persp-profiles)))
        (when (null kvs)
          (setq profile-name nil))
        (setq kvs (append kvs `((my/persp-profile-name . ,profile-name))))
        (dolist (kv kvs)
          ;; firstly, set default variable
          (set (car kv) (alist-get (car kv) default-kvs nil))
          ;; secondly, make it perspective-local
          (persp-make-variable-persp-local (car kv))
          ;; thirdly, set the profile's value
          (set (car kv) (cdr kv))))
      (force-mode-line-update))

    (defun my/persp-tab-sort-key (s)
      (if (equal s "0")
          `(10 . ,s)
        (cons (string-to-number s) s)))

    ;; hook perspective into tab-bar
    (progn
      (defun my/persp-tab-tabs (&optional frame)
        (let* ((curr (persp-curr frame))
               (persps (perspectives-hash frame)))
          (mapcar (lambda (name)
                    (let* ((p (gethash name persps))
                           (local-vars (persp-local-variables p)))
                      `(,(if (eq curr p) 'current-tab 'tab)
                        (name . ,(concat name " "
                                         (when-let* ((profile (car (alist-get 'my/persp-profile-name local-vars))))
                                           (format "[%s]" profile))))
                        (persp-name . ,name))))
                  ;; remove GLOBAL
                  (sort (delete persp-frame-global-perspective-name (hash-table-keys persps))
                        :key #'my/persp-tab-sort-key))))
      (setq tab-bar-tabs-function #'my/persp-tab-tabs)

      (defun my/persp-tab-post-select (_prev-tab tab)
        (persp-switch (alist-get 'persp-name tab)))
      (add-hook 'tab-bar-tab-post-select-functions #'my/persp-tab-post-select)

      (defun my/persp-tab-post-open (_)
        (persp-new (number-to-string (floor (time-to-seconds (current-time))))))
      (add-hook 'tab-bar-tab-post-open-functions #'my/persp-tab-post-open)

      (defun my/persp-tab-close (tab is-last-tab)
        (unless is-last-tab
          (persp-kill (alist-get 'persp-name tab))))
      (add-hook 'tab-bar-tab-pre-close-functions #'my/persp-tab-close))

    (defun my/cleanup-empty-persps ()
      ;; kill perspectives that:
      ;; 1. is not current
      ;; 2. is not initial
      ;; 3. only contains single scratch buffer
      ;; 4. the scratch buffer is not modified
      (let ((cur-name (persp-current-name))
            (all-names (persp-names))
            killed-any)
        (dolist (name all-names)
          (unless (member name `(,cur-name ,persp-initial-frame-name))
            (let ((bufs (persp-buffers (gethash name (perspectives-hash)))))
              (when (and (length= bufs 1)
                         (my/scratch-buffer-p (car bufs))
                         (not (buffer-modified-p (car bufs))))
                (persp-kill name)
                (setq killed-any t)))))
        (when killed-any
          (force-mode-line-update))))
    (add-hook 'persp-switch-hook #'my/cleanup-empty-persps)

    ;; keybindings
    (progn
      (defun my/persp-kill-current ()
        (interactive)
        (persp-kill (persp-current-name))
        (force-mode-line-update))
      (evil-define-key nil 'global
        (kbd "s-q") #'my/persp-kill-current)

      (dolist (n (number-sequence 0 9))
        (evil-define-key nil 'global
          (kbd (format "s-%d" n))
          (lambda ()
            (interactive)
            (persp-switch (format "%s" n))
            (force-mode-line-update))))
      ))

  ) ;; }}}

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
    (completion-category-overrides nil)
    (completion-category-defaults nil)
    ;; (completion-category-overrides '((file (styles basic partial-completion))))
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
    :my/env-check
    (executable-find "rg")
    :custom
    (consult-narrow-key "<")
    (consult-fd-args '("fd --full-path --color=never --type f --hidden --exclude .git"))
    ;; xref
    (xref-show-xrefs-function #'consult-xref)
    (xref-show-definitions-function #'consult-xref)
    :hook (minibuffer-setup . my/setup-minibuffer-for-consult)
    :init
    (evil-define-key '(insert emacs normal motion) 'global
      (kbd "C-t") #'my/consult-buffer-term-only
      (kbd "C-r") #'my/consult-buffer
      (kbd "C-S-r") #'my/consult-buffer-all-persp)
    (evil-define-key 'normal 'global
      (kbd "g s") #'consult-imenu  ;; LSP would integrate with imenu to provide file symbols
      (kbd "g S") #'consult-imenu-multi
      (kbd "C-h i") #'consult-info
      (kbd "C-/") #'consult-line
      (kbd "C-?") #'consult-ripgrep)
    :commands
    (my/consult-buffer
     my/consult-buffer-all-persp
     my/consult-buffer-term-only)
    :config
    (setq consult-ripgrep-args (string-replace " --search-zip" "" consult-ripgrep-args))
    (recentf-mode 1)

    (consult-customize consult-fd :initial "#^^^#")  ;; search all files, enter "fast" filter by default

    (defvar-local my/minibuffer-current-command "Current command that enters minibuffer")
    (defun my/setup-minibuffer-for-consult ()
      (setq-local completion-in-region-function #'consult-completion-in-region)
      (setq-local my/minibuffer-current-command this-command))

    (defun my/consult-change-dir ()
      (interactive)
      (let ((dir default-directory)
            (cmd my/minibuffer-current-command)
            (orig-input (ignore-errors (buffer-substring-no-properties
                                        (1+ (minibuffer-prompt-end)) (point-max)))))
        (run-at-time 0 nil
                     (lambda ()
                       (setq dir (read-directory-name "Change directory: "))
                       (setq this-command cmd)
                       (funcall cmd dir orig-input)))
        (minibuffer-quit-recursive-edit)))

    (defun my/consult-vertico-save-strip-grep-prefix ()
      (interactive)
      (when-let* ((cand (vertico--candidate)))
        (setq cand (consult--prefix-group cand 'transform))
        (setq cand (substring-no-properties cand))
        (setq cand (replace-regexp-in-string (rx bos (+ digit) ":") "" cand))
        (kill-new cand)
        (run-at-time 0 nil (lambda () (message "Copied: %s" cand)))
        (minibuffer-quit-recursive-edit)))

    (setq my/consult-grep-like-map
          (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-d") #'my/consult-change-dir)
            (define-key m (kbd "C-y") #'my/consult-vertico-save-strip-grep-prefix)
            m))

    (consult-customize consult-ripgrep :keymap my/consult-grep-like-map)
    (consult-customize consult-git-grep :keymap my/consult-grep-like-map)
    (consult-customize consult-grep :keymap my/consult-grep-like-map)
    (consult-customize consult-fd :keymap my/consult-grep-like-map)
    (consult-customize consult-find :keymap my/consult-grep-like-map)

    (defun my/consult-persp-predicate (buf)
      (or (not (boundp persp-mode))
          (not persp-mode)
          ;; does not include global perspective
          ;; because getting buffers from global perspective is slow and messes the recent buffer list
          (persp-is-current-buffer buf)))
    ;; consult buffers
    (setq my/consult--source-term-buffer
          `(
            :name "Term"
            :narrow ?t
            :category buffer
            :face consult-buffer
            :state ,#'consult--buffer-state
            :annotate ,(lambda (cand)
                         (let ((buf (get-buffer cand)))
                           (concat
                            (truncate-string-to-width
                             (or (and (buffer-local-boundp 'eat-terminal buf)
                                      (buffer-local-value 'eat-terminal buf)
                                      (eat-term-title (buffer-local-value 'eat-terminal buf)))
                                 "")
                             (floor (* 0.2 (window-body-width))) 0 ?\s)
                            "  "
                            (abbreviate-file-name (buffer-local-value 'default-directory buf)))))
            :items ,(lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name
                                :predicate 'my/consult-persp-predicate
                                :mode '(vterm-mode eat-mode)))))

    (defun my/consult-buffer-annotate (cand)
      (let ((buf (get-buffer cand)))
        (when (buffer-file-name buf)
          (abbreviate-file-name (buffer-local-value 'default-directory buf)))))
    ;; similar to consult--source-buffer, excluding terminal, limit to current perspective
    (setq my/consult--source-buffer
          `(
            :name "Buffer"
            :narrow ?b
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :default t
            :annotate ,#'my/consult-buffer-annotate
            :items ,(lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name
                                :predicate 'my/consult-persp-predicate
                                :exclude (cons (rx bos (or "*vterm" "*eat")) consult-buffer-filter)))))
    ;; similar to above, but for all perspectives (used to add buffer to current persp)
    ;; also no preview (preview would add buffer to current persp)
    (setq my/consult--source-buffer-all-persp
          `(
            :name "All Buffer"
            :narrow ?*
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :annotate ,#'my/consult-buffer-annotate
            :action ,#'consult--buffer-action
            :items ,(lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name
                                :exclude (cons (rx bos (or "*vterm" "*eat")) consult-buffer-filter)))))

    (defun my/consult-buffer ()
      (interactive)
      (let ((consult-buffer-sources '(my/consult--source-buffer
                                      consult--source-hidden-buffer
                                      consult--source-recent-file
                                      consult--source-project-buffer-hidden
                                      consult--source-project-recent-file-hidden
                                      consult--source-project-root-hidden)))
        (consult-buffer)))
    (defun my/consult-buffer-all-persp ()
      (interactive)
      (let ((consult-buffer-sources '(my/consult--source-buffer-all-persp)))
        (consult-buffer)))
    (defun my/consult-buffer-term-only ()
      (interactive)
      (let ((consult-buffer-sources '(my/consult--source-term-buffer))
            (consult--buffer-display
             (lambda (buffer-name &optional norecord)
               (if-let* ((buf (get-buffer buffer-name)))
                   (switch-to-buffer buf norecord)
                 (message "Buffer `%s' does not exists. Maybe term title changed?" buffer-name)))))
        (consult-buffer)))

    ;; from https://github.com/minad/consult/issues/318#issuecomment-882067919
    (defun my/consult-line-evil-history (&rest _)
      "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
      (when (and (bound-and-true-p evil-mode)
                 (eq evil-search-module 'evil-search))
        (let ((pattern (cadr (orderless-compile (car consult--line-history)))))
          (add-to-history 'evil-ex-search-history pattern)
          (setq evil-ex-search-pattern (list pattern t t))
          (setq evil-ex-search-direction 'forward)
          ;; do not highlight after searching. however, pressing "n" would activates them.
          ;; (when evil-ex-search-persistent-highlight
          ;;   (evil-ex-search-activate-highlight evil-ex-search-pattern))
          (evil-ex-nohighlight)
          )))

    (advice-add #'consult-line :after #'my/consult-line-evil-history))

  (use-package embark
    :custom
    (embark-mixed-indicator-delay 0.5)
    :init
    (evil-define-key '(normal motion emacs) 'global
      (kbd "C-.") #'embark-act)

    ;; evil-define-key does not work on minibuffer
    :bind (("C-." . embark-act))
    :config
    ;; reduce the list of embark-target-finders
    ;; do not find regions automatically. (ususally use region-expand or vim keybindings)
    (dolist (x '(embark-target-expression-at-point
                 embark-target-sentence-at-point
                 embark-target-paragraph-at-point
                 embark-target-defun-at-point
                 embark-target-prog-heading-at-point))
      (setq embark-target-finders (delete x embark-target-finders)))

    (defun my/embark-target-current-file ()
      "Return current file."
      (when-let* ((filename (buffer-file-name)))
        (cons 'file filename)))
    (add-to-list 'embark-target-finders 'my/embark-target-current-file 'append)

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
  (defun my/fix-word-syntax ()
    (modify-syntax-entry ?_ "w"))

  (add-hook 'prog-mode-hook #'my/fix-word-syntax)

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
    :custom (whitespace-style '(face trailing indentation space-after-tab space-before-tab
                                missing-newline-at-eof
                                ;; both tab-mark and tabs is required, for mark and mark's face respectively
                                tab-mark tabs))
    :custom-face
    (whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :italic nil
                         :inherit whitespace-space))))
    (whitespace-trailing ((t (:foreground nil :background nil :inverse-video nil
                              ;; underline's color is warning's foreground color
                              :underline (:style dots) :inherit warning)))))

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
    :hook (dired-mode . auto-revert-mode)
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
                          tramp-file-name-regexp)
                  locate-dominating-stop-dir-regexp vc-ignore-dir-regexp))

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

  (use-package dired
    :custom
    (dired-free-space nil)
    (dired-kill-when-opening-new-dired-buffer t)
    (dired-listing-switches "-alht"))

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

  (use-package ediff
    :custom
    (ediff-window-setup-function 'ediff-setup-windows-plain)
    (ediff-split-window-function 'split-window-horizontally))

  ) ;; }}}

(progn ;;; {{{  Buffer management
  (use-package midnight  ;; builtin
    :demand t
    :custom
    (midnight-delay (* 4 3600))  ;; 4am
    (clean-buffer-list-kill-regexps
     `(,(rx bos "*" (or "Man" "WoMan") " ")
       ,(rx bos "magit" (* anything) ":")))
    (clean-buffer-list-kill-buffer-names
     '("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*Ibuffer*" "*Async-native-compile-log*")))

  (use-package ibuffer  ;; builtin
    :custom
    (ibuffer-default-sorting-mode 'filename/process)
    ;; replace buffer-menu with ibuffer for evil :ls
    :init (evil-ex-define-cmd "ls" #'persp-ibuffer))

  )  ;;; }}}

(progn  ;; Coding-related packages: indent, git-gutter, .. {{{

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
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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

  ;; no config; manual activate via breadcrumb-local-mode
  (use-package breadcrumb)
  )  ;; }}}

(progn  ;; Auto-insert & snippets {{{

  (use-package tempel
    :custom
    (tempel-auto-reload nil)  ;; by default, it would check the file last-modified-time on each completion
    :hook ((prog-mode . tempel-abbrev-mode)
           (pr-review-input-mode . tempel-abbrev-mode)
           ;; this is required for tempel&abbrev to work, because abbrev would grab "word" to expand
           (tempel-abbrev-mode . my/fix-word-syntax))
    :bind (:map tempel-map
                ("TAB" . tempel-next)
                ("<backtab>" . tempel-previous))
    :commands (my/tempel-reload)
    :config
    (add-hook 'evil-insert-state-exit-hook #'tempel-done)  ;; deactivate tempel regions
    (defun my/tempel-reload ()
      (interactive)
      (let ((tempel-auto-reload t))
        (tempel-path-templates))
      (message "Reloaded templates")))

  (use-package autoinsert
    :delight auto-insert-mode
    ;; same as `auto-insert-mode' (global mode)
    ;; cannot add to major-mode-hook, because some snippet needs buffer local variables (which is load after major mode hook)
    :hook (find-file . auto-insert)
    :init
    (add-hook 'find-file-hook #'my/auto-insert-with-tempel--configure -10)  ;; before auto-insert
    (defvar my/snippet-copyright-lines nil
      "Lines of copyright header in snippet. Maybe a list of strings or a function that generate a list of strings.")
    :commands my/auto-insert-with-tempel--configure
    :config
    ;; NOTE: to customize auto-insert, add to tempel template with name "t4l_header"
    (require 'tempel)
    (defun my/auto-insert-with-tempel--do-insert ()
      (tempel-insert 't4l_header))
    (defun my/auto-insert-with-tempel--configure ()
      (when (alist-get 't4l_header (tempel--templates))
        (setq-local auto-insert-alist `((,major-mode . my/auto-insert-with-tempel--do-insert)))))

    ;; some existing items in `auto-insert-alist' are actually useful, keep them
    (defun my/keep-auto-insert-entry-p (entry)
      "Check to keep ENTRY in `auto-insert-alist'"
      (let (filename name)
        (if (consp (car entry))
            (setq filename (caar entry)
                  name (cdar entry))
          (setq filename (car entry)))
        (or (member filename '(".dir-locals.el"))
            (member name '("Emacs Lisp header" "Directory Local Variables")))))
    (setq auto-insert-alist
          (seq-filter #'my/keep-auto-insert-entry-p auto-insert-alist))

    (defun my/snippet-copyright-as-comment ()
      "Return copyright as comment string for current buffer."
      (when-let* ((lines (if (functionp my/snippet-copyright-lines)
                            (funcall my/snippet-copyright-lines)
                          my/snippet-copyright-lines)))
        (concat (mapconcat (lambda (line) (concat comment-start " " line comment-end))
                           lines "\n")
                "\n\n"))))
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
    :my/env-check
    (executable-find bazel-buildifier-command)
    :mode
    ((rx ".BUILD" eos) . bazel-build-mode)
    ("WORKSPACE\\.make\\." . bazel-workspace-mode)
    :custom (bazel-mode-buildifier-before-save t)
    :config
    ;; remove this auto-added hook. it may be slow. see bug#78545
    (remove-hook 'project-find-functions #'bazel-find-project))

  (use-package yaml-mode
    :mode (rx ".y" (? "a") "ml" eos)
    :config (add-hook 'yaml-mode-hook #'my/ensure-prog-mode))

  (use-package kotlin-mode)

  (use-package groovy-mode)

  (use-package xonsh-mode)

  (use-package markdown-mode
    :init (setq markdown-command "markdown2")
    :custom (markdown-fontify-code-blocks-natively t)
    :my/env-check
    (executable-find markdown-command)
    :config
    (defun my/markdown-mode-setup ()
      (setq-local truncate-lines nil))
    (add-hook 'markdown-mode-hook #'my/markdown-mode-setup)

    (evil-define-text-object evil-markdown-code-block (count &optional beg end type)
      "Select a markdown code block, excluding fence markers."
      (when (eq major-mode 'markdown-mode)
        ;; in gptel buffer, the syntax does not update when the buffer is modified by gptel?
        ;; a bug in gptel mode? gptel mode should not skip before-change-functions
        (syntax-ppss-flush-cache 0)
        (syntax-propertize (point-max))
        (let ((block (markdown-code-block-at-pos (point))))
          (when block
            (save-excursion
              (goto-char (nth 0 block))
              (forward-line 1)
              (let ((start (point)))
                (goto-char (nth 1 block))
                (forward-line -1)
                (evil-range start (line-end-position))))))))

    (define-key evil-inner-text-objects-map (kbd "c")
                'evil-markdown-code-block))

  (use-package go-mode
    :hook (go-mode . my/go-install-save-hooks)
    :config
    (defun my/go-install-save-hooks ()
      "Install save hooks for go."
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t)))

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

  (use-package php-mode)

  (use-package cobol-mode
    :mode ((rx "." (or "cob" "cbl" "cpy") eos) . cobol-mode))

  (add-to-list 'auto-mode-alist `(,(rx ".mm" eos) . objc-mode))
  ;; for Weixin Mini Program
  (add-to-list 'auto-mode-alist `(,(rx ".wxml" eos) . nxml-mode))
  (add-to-list 'auto-mode-alist `(,(rx ".wxss" eos) . css-mode))

  (setq python-prettify-symbols-alist '())

  (use-package kconfig-mode)

  ;; CC mode
  (use-package google-c-style
    :demand t
    :config (c-add-style "Google" google-c-style))

  (custom-set-variables
   '(c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (other . "google")))
   '(c-tab-always-indent nil))
  ) ;;; }}}

(progn  ;; Tree-sitter {{{

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
    :my/env-check (treesit-language-available-p 'typescript)
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

(progn  ;; ORG mode and note taking {{{
  (progn
    ;; bind by hydra above
    (setq my/notes-dir "~/Notes/")
    (defun my/notes-dired ()
      (interactive)
      (dired my/notes-dir))
    (defun my/notes-search ()
      (interactive)
      (let ((consult-ripgrep-args (concat consult-ripgrep-args " --sortr=modified")))
        (consult-ripgrep my/notes-dir)))
    (defun my/notes-find-file ()
      (interactive)
      (let ((this-command 'consult-fd))  ;; to make consult-customize work
        (consult-fd my/notes-dir))))

  (use-package org
    :my/env-check (file-directory-p "~/Notes/org")
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

  (use-package verb
    :demand t
    :after org
    :config
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
    (evil-define-minor-mode-key 'normal 'verb-response-body-mode
      (kbd "q") (lambda () (interactive) (verb-kill-response-buffer-and-window t)))

    (defun my/verb-run-curl ()
      (interactive)
      (when-let* ((cmd (verb--export-to-curl (verb--request-spec-from-hierarchy)
                                            'no-message 'no-kill)))
        (setq cmd (replace-regexp-in-string "^curl" "curl -v" cmd))
        ;; write cmd into file and execute; to 1. show cmd in output buffer; 2. reduce "finished" message size
        (let ((tmpfile (make-temp-file "verb-curl-" nil ".sh"
                                       (concat "echo \"$0\"; cat \"$0\"\n" cmd "\n"))))
          (set-file-modes tmpfile #o755)
          (async-shell-command tmpfile "*verb curl output*"))))
    ;; the output buffer derives from comint-mode, so evil uses insert state by default
    (add-to-list 'evil-buffer-regexps '("^\\*verb curl output\\*" . normal))

    (define-key verb-command-map (kbd "C-c") #'my/verb-run-curl))

  ;; define outside of "use-package verb" because that only loads after org
  (defun verb ()
    "Create a new `verb-mode' buffer from example."
    (interactive)
    (with-current-buffer (generate-new-buffer "*verb*")
      (insert-file-contents (file-name-concat user-emacs-directory "examples/verb.org"))
      (org-mode)
      (verb-mode)
      (switch-to-buffer-other-window (current-buffer))))

  )  ;;; }}}

(progn  ;; Terminal {{{
  (use-package with-editor
    :commands with-editor)

  (defun my/term-process-kill-buffer-query-function ()
    (let* ((default-directory "/")  ;; avoid listing processes from remote host
           (process (get-buffer-process (current-buffer))))
      (or (not process)
          (not (memq major-mode '(vterm-mode eat-mode)))
          (not (memq (process-status process) '(run stop open listen)))
          ;; does not have any subprocess
          (not (member (process-id process)
                       (mapcar (lambda (p) (alist-get 'ppid (process-attributes p)))
                               (list-system-processes))))
          (yes-or-no-p (format "Term %S has a running subprocess; kill it? "
                               (buffer-name (current-buffer)))))))
  (defun my/term-process-kill-emacs-query-function ()
    (seq-every-p (lambda (buf)
                   (with-current-buffer buf
                     (my/term-process-kill-buffer-query-function)))
                 (buffer-list)))

  (add-hook 'kill-buffer-query-functions #'my/term-process-kill-buffer-query-function)
  (add-hook 'kill-emacs-query-functions #'my/term-process-kill-emacs-query-function)

  (defun my/term-set-cwd (pwd)
    "Similar to `cd-absolute', but without check."
    (setq default-directory pwd
          list-buffers-directory pwd))

  ;; https://github.com/akermu/emacs-libvterm/issues/746
  ;; also required for eat for similar reasons
  (defun my/wrap-deferred (fn)
    (lambda (&rest args)
      (let ((buf (current-buffer)))
        (run-with-timer 0.001 nil
                        (lambda ()
                          (with-current-buffer buf
                            (apply fn args)))))))

  (setq my/term-cmds `(("man" . ,(my/wrap-deferred 'man))
                       ("magit-status" . ,(my/wrap-deferred 'magit-status))
                       ("rg-run-raw" . ,(my/wrap-deferred 'my/rg-run-raw))
                       ("woman-find-file" . ,(my/wrap-deferred 'woman-find-file-with-fallback))
                       ("find-file" . ,(my/wrap-deferred 'my/find-file-fallback-sudo))
                       ("set-cwd" . my/term-set-cwd)))

  (defalias 'my/term 'my/eat)

  (evil-ex-define-cmd "term" #'my/term)
  (evil-define-key '(normal motion emacs) 'global
    (kbd "<C-return>") #'my/term)

  (use-package eat
    :my/env-check (executable-find "xonsh")
    :custom
    (eat-kill-buffer-on-exit t)
    (eat-shell (or (executable-find "xonsh") shell-file-name))
    (eat-enable-mouse nil)
    (eat-enable-shell-prompt-annotation nil)
    ;; disable the default process-kill-buffer-query-function
    ;; see above my/term-process-kill-buffer-query-function
    (eat-query-before-killing-running-terminal nil)
    (eat-term-scrollback-size (* 64 10000))  ;; chars. ~10k lines?
    (eat-message-handler-alist my/term-cmds)
    :commands (my/eat)
    :config
    ;; this function is written by Claude
    (defun my/generate-docker-style-name ()
      "Generate a random two-word name similar to Docker container names, using simpler words."
      (let ((adjectives '("big" "blue" "bold" "brave" "busy" "calm" "cool" "dear" "dry" "fair"
                          "fast" "fine" "firm" "flat" "free" "fresh" "glad" "good" "green" "happy"
                          "hard" "hot" "kind" "late" "light" "long" "loud" "low" "new" "nice"
                          "odd" "old" "pink" "proud" "pure" "quick" "red" "rich" "safe" "sharp"
                          "shy" "small" "soft" "sweet" "tall" "tiny" "warm" "wet" "wild" "wise"
                          "young"))
            (nouns '("ant" "apple" "bear" "bee" "bird" "boat" "book" "box" "boy" "cat"
                     "chair" "clock" "cloud" "cow" "desk" "dog" "door" "duck" "face" "fish"
                     "flag" "flower" "car" "girl" "hat" "horse" "house" "king" "lake" "lion"
                     "moon" "mouse" "park" "pen" "road" "rock" "rose" "ship" "sky" "snake"
                     "star" "sun" "table" "tree" "watch" "whale")))
        (format "%s-%s"
                (nth (random (length adjectives)) adjectives)
                (nth (random (length nouns)) nouns))))

    ;; this function is written by Claude
    (defun my/generate-unique-eat-name ()
      "Generate a unique EAT buffer name with Docker-style suffix.
Returns a string like '*eat*<fun-girl>' that doesn't clash with existing buffers."
      (let (buffer-name)
        (while (or (null buffer-name)
                   (get-buffer buffer-name))
          (setq buffer-name (format "*eat*<%s>" (my/generate-docker-style-name))))
        buffer-name))

    (defun my/eat ()
      "Similar to eat, but always create a new buffer, and setup proper envvars."
      (interactive)
      (let ((default-directory default-directory))
        (when (file-remote-p default-directory)
          (setq default-directory "~/"))
        (let* ((program (funcall eat-default-shell-function))
               (buf (generate-new-buffer (my/generate-unique-eat-name)))
               (emacs-dir (expand-file-name user-emacs-directory))
               ;; PAGER: https://github.com/akermu/emacs-libvterm/issues/745
               (process-environment
                (append '("PAGER"
                          "EDITOR=emacsclient-on-current-server")
                        process-environment)))
          (with-current-buffer buf
            (eat-mode)
            (pop-to-buffer-same-window buf)
            (eat-exec buf (buffer-name) "/usr/bin/env" nil (list "sh" "-c" (concat "exec " program)))))))

    (evil-define-key '(insert emacs) eat-mode-map
      (kbd "C-S-v") #'eat-yank
      (kbd "C-q") #'eat-quoted-input
      ;; make sure to send following keys to terminal
      (kbd "C-w") #'eat-self-input
      (kbd "C-a") #'eat-self-input
      (kbd "C-b") #'eat-self-input
      (kbd "C-c") #'eat-self-input
      (kbd "C-d") #'eat-self-input
      (kbd "C-e") #'eat-self-input
      (kbd "C-f") #'eat-self-input
      (kbd "C-l") #'eat-self-input
      (kbd "C-o") #'eat-self-input
      (kbd "C-p") #'eat-self-input
      (kbd "C-n") #'eat-self-input)
    (evil-define-key 'normal eat-mode-map
      (kbd "C-j") #'eat-next-shell-prompt
      (kbd "C-k") #'eat-previous-shell-prompt)

    (defun my/eat-sync-evil-state ()
      ;; eat-char-mode (all keys sent to terminal):
      ;;   - evil insert mode  (ESC would exit to normal mode)
      ;;   - evil emacs mode  (ESC would be sent to terminal)
      ;; eat-emacs-mode (all keys sent to emacs):
      ;;   - evil normal/... mode
      ;;   (evil normal + eat-char-mode does not work)
      (if (memq evil-next-state '(insert emacs))
          (progn
            (setq-local eat-term-scrollback-size (default-value 'eat-term-scrollback-size))
            (setq-local truncate-lines t)
            (eat-char-mode)
            (goto-char (eat-term-display-cursor eat-terminal)))
        ;; eat-term-scrollback-size: do not clear scrollback on normal mode
        (setq-local eat-term-scrollback-size nil)
        ;; enable wrapping (no truncate-lines) only on normal mode (only affects scrollback region)
        ;; because for some unknown reason, if wrapping is enabled in insert mode, the window scroll position would flicker
        (setq-local truncate-lines nil)
        (eat-emacs-mode)))

    (defun my/eat-setup (proc)
      (dolist (hook '(evil-insert-state-entry-hook
                      evil-insert-state-exit-hook
                      evil-emacs-state-entry-hook
                      evil-emacs-state-exit-hook))
        (add-hook hook #'my/eat-sync-evil-state 0 'local))

      (eat-char-mode)
      ;; don't know why, but this is required. evil-set-initial-state is not enough,
      ;; the keybindings in insert state only works after explicitly calling this.
      (evil-insert-state))

    ;; use eat-exec-hook instead of eat-mode-hook,
    ;; eat-exec-hook happens later than eat-mode-hook.
    ;; we cannot call eat-char-mode etc., in eat-mode-hook
    (add-hook 'eat-exec-hook #'my/eat-setup)

    ;; FIXME(yikai): without this, pressing ESC in eat may not go back one char, which make the cursor unable to move backward
    ;; only happens on moonshot macbook?? wtf
    ;; caused by constrain-to-field in evil-move-cursor-back
    (my/define-advice evil-move-cursor-back (:around (old-fn &rest args) fix-eat-cursor-go-back)
      (let ((inhibit-field-text-motion (eq major-mode 'eat-mode)))
        (apply old-fn args)))
    )

  )  ;; }}}

(progn  ;; Project / Window management {{{
  (use-package find-file
    :custom
    (ff-ignore-include t)
    (cc-other-file-alist
     ;; modified so that .h is the first item, which will be created when not found
     `((,(rx "." (or "c" "cc" "c++" "cpp" "cxx" "CC" "C" "C++" "CPP" "CXX") eos)
        (".h" ".hh" ".hpp" ".hxx" ".H" ".HPP" ".HH"))
       (,(rx "." (or "h" "hh" "hpp" "hxx" "H" "HPP" "HH") eos)
        (".cc" ".c" ".cxx" ".cpp" ".c++" ".CC" ".C" ".CXX" ".CPP" ".C++")))))

  (use-package project
    :demand t
    :init
    (evil-define-key '(normal motion emacs visual) 'global
      (kbd "C-p") project-prefix-map)
    :bind (:map project-prefix-map
                ("f" . consult-fd)
                ("h" . ff-find-other-file)
                ("/" . consult-ripgrep))
    :custom
    (project-mode-line t)
    :config
    (defvar-local my/project-cache nil
      "Cached result of `my/project-try'.
nil means not cached;
otherwise it should be '(dir . value). (value may be nil).
dir is the directory of the buffer (param of my/project-try), when it's changed, the cache is invalidated")

    (defun my/project-try (dir)
      (cond
       ((equal (car my/project-cache) dir)
        (cdr my/project-cache))
       ((file-remote-p dir)
        nil)
       (t
        (let* ((try-markers '((".dir-locals.el" ".projectile" ".project" "WORKSPACE")
                              (".git" ".svn")))
               (res (cl-loop for markers in try-markers
                             for root = (locate-dominating-file
                                         dir
                                         (lambda (x)
                                           (seq-some
                                            (lambda (marker) (file-exists-p (expand-file-name marker x)))
                                            markers)))
                             when root return (cons 'my/proj root))))
          (setq-local my/project-cache (cons dir res))  ;; res may be nil, but also cache
          res))))

    ;; NOTE: Remove the default #'project-try-vc! Only keep my own version. The default implementation is slow.
    ;; bug#78545
    (setq project-find-functions (list #'my/project-try))

    (cl-defmethod project-root ((project (head my/proj)))
      (cdr project))

    (cl-defmethod project-name ((project (head my/proj)))
      ;; support for pony style project name (.sub-repos)
      (or (let* ((dir (project-root project))
                 (parts (nreverse (string-split dir "/" t))))
            (when (and (length> parts 1)
                       (string-prefix-p "." (car parts)))
              (format "%s/%s" (cadr parts) (car parts))))
          (cl-call-next-method)))

    (defun my/current-project-root ()
      (when-let* ((p (project-current)))
        (project-root p))))

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
                             (company-capf :with company-abbrev :separate)
                             ;; company-abbrev backend is for tempel (tempel-abbrev-mode is on)
                             (company-dabbrev-code
                              ;; removed for slow performance
                              ;; company-gtags company-etags
                              company-keywords
                              company-abbrev)
                             ;; company-dabbrev
                             )
          company-transformers '(my/company-sort-adjust-abbrev))

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
    :commands (my/company-sort-adjust-abbrev)
    :config
    ;; (see above) since the backends is set to (company-capf :with company-abbrev :separate),
    ;; the abbrev candidates (for tempel) would always in the bottom,
    ;; even if they are the exact matches while the CAPF candidates are not.
    ;; on the other hand, when used together with company-dabbrev-code and keywords, abbrev candidates would sometimes in the top unnecessarily.
    ;; The following function would try to bringup those candidates if >= 3 chars match;.
    ;; but bring them down if < 3 chars match.
    (defun my/company-sort-adjust-abbrev--score (c)
      (if (eq (get-text-property 0 'company-backend c) 'company-abbrev)
          ;; when this function is called in company-calculate-candidates,
          ;; company-prefix is actually not updated yet. so when this condition is true, the prefix is actually >= 3 chars
          (if (length> company-prefix 1)
              -10
            10)
        0))
    (defun my/company-sort-adjust-abbrev (candidates)
      (sort candidates  ;; the sort is stable
            (lambda (c1 c2)
              (< (my/company-sort-adjust-abbrev--score c1) (my/company-sort-adjust-abbrev--score c2)))))

    ;; TODO: needs more improvement
    (evil-define-key 'insert comint-mode-map
      (kbd "TAB") #'company-complete)
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
      (kbd "TAB") 'company-complete-selection
      (kbd "<tab>") 'company-complete-selection  ;; this is required because company-active-map set both "TAB" and <tab> by default
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
    (evil-define-minor-mode-key 'insert 'company-mode
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

    (evil-define-minor-mode-key 'insert 'corfu-mode
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

(progn  ;; Checking. Flycheck, jinx  {{{
  (use-package flycheck
    :custom
    (flycheck-python-pylint-executable "pylint")
    (flycheck-emacs-lisp-load-path 'inherit)
    (flycheck-disabled-checkers '(python-ruff))  ;; use lsp for ruff
    (flycheck-mode-line-color nil)
    :hook (prog-mode-local-only . flycheck-mode)
    :config
    (require 'warnings)
    (add-to-list 'warning-suppress-types '(flycheck))  ;; prevent warnings from flycheck-disable-excessive-checker

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
    :my/env-check (executable-find flycheck-c/c++-googlelint-executable)
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
    :when (display-graphic-p)
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config
    ;; https://github.com/alexmurray/flycheck-posframe/issues/25
    (setq flycheck-posframe-hide-posframe-hooks nil
          flycheck-posframe-timeout 0.0
          flycheck-display-errors-delay 0.2
          ;; make it hidden
          flycheck-posframe-buffer " *flycheck-posframe-buffer*")

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

  (comment jinx
    :custom
    (jinx-languages "en_US")
    :config
    (define-key jinx-overlay-map
                (kbd "g x") #'jinx-correct))

  )  ;; }}}

(comment  ;; Flymake  {{{
  (use-package flymake-posframe
    :hook (flymake-mode . flymake-posframe-mode))
  )  ;; }}}

(progn  ;; LSP-mode  {{{
  (use-package lsp-mode
    :my/env-check (executable-find "emacs-lsp-booster")
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
           (protobuf-mode . my/maybe-start-lsp)

           (lsp-mode . lsp-enable-which-key-integration))
    :commands (lsp lsp-deferred lsp-find-session-folder)
    :delight
    '(" #"
      (lsp--buffer-workspaces
       (:eval (mapconcat (lambda (w) (symbol-name (lsp--workspace-server-id w)))
                         lsp--buffer-workspaces "/"))
       (:propertize "?" face warning)))
    :config
    (unless lsp-use-plists
      (error "`lsp-use-plists' is not set!"))
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
              (when-let* ((command-from-exec-path (executable-find (car orig-result))))
                (setcar orig-result command-from-exec-path))
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

  (with-eval-after-load 'lsp-mode
    (defvar lsp-typos-lsp-config-path nil)
    (defvar lsp-typos-lsp-diagnostic-severity "Info")  ;; Error, Warning, Info or Hint.
    (defvar lsp-typos-lsp-command '("typos-lsp"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () lsp-typos-lsp-command))
      :server-id 'typos-lsp
      :add-on? t
      :activation-fn (lambda (filename mode) (provided-mode-derived-p mode '(prog-mode)))
      :priority -10
      :initialization-options
      (lambda ()
        (append
         `(:diagnosticSeverity ,lsp-typos-lsp-diagnostic-severity)
         (when lsp-typos-lsp-config-path
           `(:config ,lsp-typos-lsp-config-path))))
      )))

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
    ;; FIXME: should only parse known domain
    ;; (add-to-list 'browse-url-default-handlers
    ;;              '(pr-review-url-parse . pr-review-open-url))
    :config
    (setq pr-review-generated-file-regexp
          (rx (or (: (*? anychar) "generated/" (* anychar))
                  (: (*? anychar) (or "pdm.lock" "pdm_lock.bzl") eol))))
    (defun my/pr-review-input-buffer-set-company ()
      (set (make-local-variable 'company-backends)
           ;; emoji, tempel
           '(company-emoji company-abbrev)))
    (add-hook 'pr-review-input-mode-hook #'my/pr-review-input-buffer-set-company))

  (use-package browse-at-remote
    :init
    (evil-define-key '(normal visual) 'global
      (kbd "C-c l") #'my/hydra-bar/body)
    :commands (my/hydra-bar/body)
    :config
    (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^github\\.corp\\..*" :type "github"))
    (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^dev\\.msh\\.team" :type "gitlab"))

    (require 'hydra)
    (defun my/hydra-bar-get-url ()
      (let ((browse-at-remote-preferred-remote-name my/hydra-bar-var/preferred-remote-name)
            (browse-at-remote-prefer-symbolic my/hydra-bar-var/prefer-symbolic)
            (browse-at-remote-add-line-number-if-no-region-selected my/hydra-bar-var/add-line-number-if-no-region-selected))
        (browse-at-remote-get-url)))
    (defhydra my/hydra-bar
      (nil nil
           :hint nil
           :color red
           :pre (when (eq this-command 'my/hydra-bar/body)
                  (setq my/hydra-bar-var/preferred-remote-name browse-at-remote-preferred-remote-name
                        my/hydra-bar-var/prefer-symbolic browse-at-remote-prefer-symbolic
                        my/hydra-bar-var/add-line-number-if-no-region-selected browse-at-remote-add-line-number-if-no-region-selected)))
      "
Browse at remote
================

[_r_] Remote: %`my/hydra-bar-var/preferred-remote-name
[_c_] Use commit: %(not my/hydra-bar-var/prefer-symbolic)
[_n_] Line number: %s(cond ((use-region-p) \"range\") (my/hydra-bar-var/add-line-number-if-no-region-selected \"single\") (t \"nil\"))

Preview: %s(my/hydra-bar-get-url)

"
      ("r" (setq my/hydra-bar-var/preferred-remote-name (read-from-minibuffer "Remote: ")))
      ("c" (setq my/hydra-bar-var/prefer-symbolic (not my/hydra-bar-var/prefer-symbolic)))
      ("n" (setq my/hydra-bar-var/add-line-number-if-no-region-selected (not my/hydra-bar-var/add-line-number-if-no-region-selected)))
      ("l" (kill-new (my/hydra-bar-get-url)) "Copy link" :color blue)
      ("o" (browse-url (my/hydra-bar-get-url)) "Open in browser" :color blue)))

  (use-package rg
    :my/env-check (executable-find "rg")
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
          (rg-mode-init search))))

    ;; for some unknown reason, the on-the-fly parsing may fail (seems to have something to do with eat & process filter)
    ;; anyway, ensure parsing at the end of running
    (defun my/rg-ensure-parse-all (buf msg)
      (with-current-buffer buf
        (when (eq major-mode 'rg-mode)
          (save-excursion
            (compilation-parse-errors (point-min) (point-max))))))
    (defun my/rg-mode-setup ()
      (add-hook 'compilation-finish-functions #'my/rg-ensure-parse-all 0 'local))

    (add-hook 'rg-mode-hook #'my/rg-mode-setup))

  (use-package mac-input-source
    :when (my/macos-p)
    :demand t)

  (progn
    (require 'dbus nil 'noerror)
    ;; chinese input method integration. switch off IM when leaving insert state.
    ;; Difference between fcitx.el:
    ;; - we use switch-buffer-functions instead of advicing switch-to-buffer,
    ;;   so that if a command invokes multiple switch-to-buffer function, only one switch is performed.
    ;; - I only use these functions anyway.
    ;; - Most of these functions in fcitx are contributed by me anyway.
    (defun my/im-switch (active-p) nil)
    (defun my/im-active-p () nil)
    ;; define implementations based on OS env
    (cond
     ((and (my/macos-p) (fboundp 'mac-input-source))
      (setq my/fcitx-macos-im-name "com.apple.inputmethod.SCIM.Shuangpin")
      (defun my/im-switch (active-p)
        (mac-select-input-source
         (if active-p my/fcitx-macos-im-name 'ascii-capable-keyboard)))
      (defun my/im-active-p ()
        (null (cdr (mac-input-source nil :ascii-capable-p))))
      t)
     ((and (fboundp 'dbus-ping)
       (dbus-ping :session "org.fcitx.Fcitx5"))
      (defun my/im-switch (active-p)
        (dbus-call-method :session
                          "org.fcitx.Fcitx5"
                          "/controller"
                          "org.fcitx.Fcitx.Controller1"
                          (if active-p "Activate" "Deactivate")))
      (defun my/im-active-p ()
        (= 2 (dbus-call-method :session
                               "org.fcitx.Fcitx5"
                               "/controller"
                               "org.fcitx.Fcitx.Controller1"
                               "State")))
      t)
     ((and (fboundp 'dbus-ping)
           (dbus-ping :session "org.fcitx.Fcitx"))
      (defun my/im-switch (active-p)
        (dbus-call-method :session
                          "org.fcitx.Fcitx"
                          "/inputmethod"
                          "org.fcitx.Fcitx.InputMethod"
                          (if active-p "ActivateIM" "InactivateIM")))
      (defun my/im-active-p ()
        (= 2 (dbus-call-method :session
                               "org.fcitx.Fcitx"
                               "/inputmethod"
                               "org.fcitx.Fcitx.InputMethod"
                               "GetCurrentState")))
      t))

    (defvar-local my/im-buffer-active-during-insert nil
      "Whether current buffer should have active IM during insert state")
    (defun my/im-buffer-leave-insert ()
      (let ((active-p (my/im-active-p)))
        (setq-local my/im-buffer-active-during-insert active-p)
        (when active-p
          (my/im-switch nil))))
    (defun my/im-buffer-enter-insert ()
      (unless (eq my/im-buffer-active-during-insert (my/im-active-p))
        (my/im-switch my/im-buffer-active-during-insert)))
    (add-hook 'evil-insert-state-entry-hook #'my/im-buffer-enter-insert)
    (add-hook 'evil-insert-state-exit-hook #'my/im-buffer-leave-insert)

    (defun my/im-buffer-on-switch (old-buf new-buf)
      (when (and old-buf (buffer-live-p old-buf))
        (with-current-buffer old-buf
          (when (evil-insert-state-p)
            (my/im-buffer-leave-insert))))
      (when (and new-buf (buffer-live-p new-buf))
        (with-current-buffer new-buf
          (when (evil-insert-state-p)
            (my/im-buffer-enter-insert)))))
    (add-hook 'switch-buffer-functions #'my/im-buffer-on-switch))

  (use-package xref  ;; builtin
    :config
    ;; never use etags in xref
    (remove-hook 'xref-backend-functions #'etags--xref-backend))

  (use-package dumb-jump
    :my/env-check (progn "rg must support pcre2"
                         (string-match-p (rx "+pcre2") (shell-command-to-string "rg --version")))
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

    (evil-add-command-properties #'dumb-jump-go :jump t)
    (my/define-advice dumb-jump-get-project-root (:override (filepath) use-project)
      (s-chop-suffix "/" (expand-file-name
                          (or (my/current-project-root)
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
    ;; we don't want to use with-editor because it would add process filter
    ;; (for its fallback sleeping editor) which is slow
    (unless (process-live-p server-process)
      (when (server-running-p server-name)
        (setq server-name (format "server%s" (emacs-pid)))
        (when (server-running-p server-name)
          (server-force-delete server-name)))
      (server-start))
    (setenv "EMACS_SERVER_SOCKET" (expand-file-name server-name server-socket-dir)))

  (use-package man
    :init
    (when (my/macos-p)
      (setq Man-sed-command "gsed"
            Man-awk-command "gawk"))
    (evil-define-key '(normal motion) 'global
      (kbd "C-h M") #'man)
    :custom (Man-notify-method 'pushy)
    :commands man
    :config
    (evil-add-command-properties #'man-follow :jump t))

  (use-package woman
    :custom (woman-fill-frame t)
    :commands woman-find-file-with-fallback
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
                ((and (url-host url)
                      (url-filename url)
                      (string-match-p (rx (or "." bos) "google.com") (url-host url))
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
            (kbd "g L") #'browse-url-firefox))

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
      (kbd "g o") #'devdocs-browser-eww-open-in-default-browser)
    :config
    (setf (alist-get 'python-mode devdocs-browser-major-mode-docs-alist)
          '("Python" "NumPy" "PyTorch")))

  (use-package w3m
    :custom
    (w3m-display-mode 'plain)
    (w3m-confirm-leaving-secure-page nil)
    (w3m-session-crash-recovery nil)
    (w3m-session-load-crashed-sessions nil)
    (w3m-session-load-last-sessions nil)
    :config
    (setq my/w3m-man2html-cgi-path
          (cl-loop for dir in '("libexec" "lib")
                   for path = (expand-file-name
                               (concat "../" dir "/w3m/cgi-bin/w3mman2html.cgi")
                               (file-name-directory w3m-command))
                   if (file-exists-p path) return path))
    ;; TODO: submit a patch?
    (my/define-advice w3m-url-local-p (:before-while (url) treat-w3mman-as-nonlocal)
      (not (string-match "w3mman2html\\.cgi" url)))

    ;; (w3m (concat "file://"
    ;;              ;; my/w3m-man2html-cgi-path
    ;;              "/$LIB/w3mman2html.cgi"
    ;;              "?local=/usr/share/man/man1/ls.1&quit=ok"
    ;;              ;; "?man=rsync"
    ;;              )
    ;;      'new-session 'interactive)
    )

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
    (auth-source-save-behavior nil)
    :config
    (auth-source-pass-enable)
    (add-to-list 'auth-sources
                 (expand-file-name "secrets/lowrisk.authinfo.gpg" user-emacs-directory)
                 'append)
    :my/env-check
    (file-exists-p auth-source-pass-filename))

  )  ;; }}}

(progn  ;; AI {{{

  (use-package gptel
    :my/env-check
    (gptel-api-key-from-auth-source "openrouter.ai")
    :commands (my/hydra-gptel/body gptel-api-key-from-auth-source)
    :init
    ;; hack: set gptel--openai before loading package, to prevent it making openai backend by default
    ;; (after making the backend, it will be shown while selecting models)
    (setq gptel--openai nil)
    (evil-define-key '(normal visual) 'global
      (kbd "C-a") #'my/hydra-gptel/body)
    :config
    (setq gptel-expert-commands t
          gptel-default-mode 'markdown-mode
          gptel-temperature nil  ;; use service default value
          ;; default "scope: buffer" in gptel-menu
          gptel--set-buffer-locally t)

    ;; gptel-proxy does not support username/password
    (when my/curl-proxy
      (setq gptel-curl-extra-args `("-x" ,my/curl-proxy)))

    (let* ((openrouter-params (list :host "openrouter.ai"
                                    :endpoint "/api/v1/chat/completions"
                                    :key (gptel-api-key-from-auth-source "openrouter.ai"))))
      (setq my/gptel-backend-openrouter (apply #'gptel-make-openai "OpenRouter"
                                               :models '(openai/gpt-4o openai/o4-mini openai/gpt-4o-search-preview
                                                         anthropic/claude-3.7-sonnet anthropic/claude-sonnet-4
                                                         google/gemini-2.5-pro google/gemini-2.5-flash)
                                               :stream t
                                               openrouter-params)
            ;; use make-perplexity and disable streaming to support citations.
            my/gptel-backend-perplexity (apply #'gptel-make-perplexity "OpenRouter/Perplexity"
                                               :models '(perplexity/sonar perplexity/sonar-pro)
                                               :stream nil
                                               openrouter-params)))
    ;; google aistudio blocks chinese ip; vertex ai uses complex auth flow. so use my vertexai proxy in cloudflare
    (let* ((gemini-host "vertexai-gemini-cf-workers.blahgeek.workers.dev")
           (gemini-params (list :key (gptel-api-key-from-auth-source gemini-host)
                                :host gemini-host
                                :endpoint "/v1/models"
                                ;; https://cloud.google.com/vertex-ai/generative-ai/docs/models/gemini/2-5-pro
                                :models '(gemini-2.5-flash gemini-2.5-pro gemini-2.0-flash)
                                :stream t)))
      (setq my/gptel-backend-gemini-with-code
            (apply #'gptel-make-gemini "Gemini (with code)"
                   :request-params '(:tools [(:code_execution ())])
                   gemini-params)
            my/gptel-backend-gemini-with-search
            (apply #'gptel-make-gemini "Gemini (with search)"
                   :request-params '(:tools [(:google_search ())])
                   gemini-params)))

    ;; https://platform.moonshot.cn/docs/guide/use-web-search
    ;; it requires tool response just like regular tool, so we need to set `gptel-tools'. see my/new-gptel-buffer below.
    ;; also, the :type should be "builtin_function", so use :request-params to override.
    (setq my/gptel-tool-moonshot-search
          (gptel-make-tool
           :name "$web_search"
           :function (lambda (&optional search_result) (json-serialize `(:search_result ,search_result)))
           :description "Moonshot builtin web search. Only usable by moonshot model (kimi), ignore this if you are not."
           :args '((:name "search_result" :type object :optional t))
           :confirm nil
           :category "web"))
    (setq my/gptel-backend-moonshot-with-search
          (let ((host (if (getenv "INSIDE_MSH_TEAM") "api.msh.team" "api.moonshot.cn")))
            (gptel-make-openai "Moonshot (with search)"
              :host host
              :key (gptel-api-key-from-auth-source host)
              :stream t
              :models '(kimi-k2-turbo-preview kimi-latest)
              :request-params '(:tools [(:type "builtin_function" :function (:name "$web_search"))]))))

    (setq gptel-backend my/gptel-backend-openrouter  ;; set openai as default
          gptel-model (car (gptel-backend-models gptel-backend)))

    (evil-define-minor-mode-key '(normal insert) 'gptel-mode
      (kbd "C-c C-c") #'gptel-send
      (kbd "C-c <C-m>") #'gptel-menu
      (kbd "C-c C-s") #'gptel-menu
      (kbd "C-c C-k") #'kill-current-buffer)

    (my/define-advice gptel-request (:before (&rest _) goto-eob)
      "Goto end of buffer before sending while in `gptel-mode'."
      (when gptel-mode
        (goto-char (point-max))))


    (defun my/new-gptel-buffer (backend &optional model)
      "Create new gptel buffer with BACKEND and MODEL."
      (unless backend
        (user-error "Backend not available"))
      ;; use first model for backend as default
      (unless model
        (setq model (car (gptel-backend-models backend))))
      (unless (member model (gptel-backend-models backend))
        (user-error "Unknown model %s" model))
      (let* ((bufname (generate-new-buffer-name "*gptel*"))
             (region (when (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))))
             (buf (gptel bufname nil
                         (when region
                           (concat (alist-get gptel-default-mode gptel-prompt-prefix-alist)
                                   "\n\n" region))
                         'interactive)))
        (with-current-buffer buf
          (goto-char (point-min))
          (move-end-of-line nil)
          (setq-local gptel-backend backend
                      gptel-model model)
          (when (eq backend my/gptel-backend-moonshot-with-search)
            (setq-local gptel-tools (list my/gptel-tool-moonshot-search))))))

    (require 'hydra)
    (defhydra my/hydra-gptel
      (nil nil :exit t :color blue :hint nil)
      "
AI!
=======

^Chat in buffer^^^                      ^Action^            ^Aider^
^-^-------------^-^-------------        ^-^-------          ^---^-------
_i_: ChatGPT    _k_: Kimi               _m_: Menu           _C-a_: Menu
_s_: Search     _p_: Perplexity         _r_: Rewrite
_c_: Coding
"
      ("i" (my/new-gptel-buffer my/gptel-backend-openrouter 'openai/gpt-4o))
      ("s" (my/new-gptel-buffer my/gptel-backend-gemini-with-search))
      ("p" (my/new-gptel-buffer my/gptel-backend-perplexity))
      ("c" (my/new-gptel-buffer my/gptel-backend-openrouter 'anthropic/claude-sonnet-4))
      ("k" (my/new-gptel-buffer my/gptel-backend-moonshot-with-search))
      ("r" gptel-rewrite)
      ("m" gptel-menu)
      ("C-a" aider-transient-menu))
    )

  (use-package plz
    :config
    (when my/curl-proxy
      (unless (member "-x" plz-curl-default-args)
        (setq plz-curl-default-args (append plz-curl-default-args (list "-x" my/curl-proxy))))))

  (use-package minuet
    :custom
    (minuet-request-timeout 5)
    :init
    (evil-define-key 'insert prog-mode-map
      (kbd "C-f") #'minuet-show-suggestion)
    (evil-define-minor-mode-key 'insert 'minuet-active-mode
      (kbd "C-j") #'minuet-next-suggestion
      (kbd "C-k") #'minuet-previous-suggestion
      (kbd "C-f") #'minuet-accept-suggestion
      (kbd "C-S-f") #'minuet-accept-suggestion-line)

    :config
    (require 'gptel)
    (require 'company)

    (add-hook 'evil-insert-state-exit-hook #'minuet-dismiss-suggestion)
    (add-hook 'minuet-active-mode-hook #'company-abort)

    (setq minuet-provider 'openai-compatible)
    (plist-put minuet-openai-compatible-options
               :end-point "https://openrouter.ai/api/v1/chat/completions")
    (plist-put minuet-openai-compatible-options
               :model "google/gemini-2.0-flash-001")
    (plist-put minuet-openai-compatible-options
               :api-key (lambda () (gptel-api-key-from-auth-source "openrouter.ai")))
    ;; (plist-put minuet-openai-options
    ;;            :api-key (lambda () (gptel-api-key-from-auth-source "api.openai.com")))
    )

  (use-package aider
    ;; see my/hydra-gptel above for keybinding
    :custom
    (aider-auto-trigger-command-completion nil)
    :config
    (defun my/aider-comint-on-insert-mode ()
      (goto-char (point-max)))

    (defun my/aider-comint-mode-setup ()
      "Setup for aider-comint-mode."
      (setq-local truncate-lines nil
                  comint-prompt-read-only t)
      (add-hook 'evil-insert-state-entry-hook #'my/aider-comint-on-insert-mode 0 'local)
      (setq-local company-backends '(company-capf))  ;; default is company-files
      )
    (add-hook 'aider-comint-mode-hook #'my/aider-comint-mode-setup))

  (comment codeium
    :my/env-check (codeium-get-saved-api-key)
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

  (use-package notmuch  ;; notmuch requires version match between elisp code and CLI. so it's not included in submodule. use system version.
    :init
    (evil-ex-define-cmd "nm" #'notmuch)
    :custom
    (notmuch-search-oldest-first nil)
    (notmuch-show-logo nil)
    (notmuch-archive-tags '("-inbox" "-unread"))
    :commands notmuch
    :config
    (dolist (x '(("gh-.*" . notmuch-tag-unread)
                 ("g-.*" . notmuch-tag-unread)
                 (".*-merged" . success)))
      (add-to-list 'notmuch-tag-formats
                   `(,(car x) (propertize tag 'face ',(cdr x)))))
    ;; display text/html for github notifications
    (defun my/notmuch-multipart/alternative-discouraged (msg)
      (if (string-match-p "@github" (plist-get msg :id))
          '("text/plain")
        '("text/html" "multipart/related")))
    (setq notmuch-multipart/alternative-discouraged #'my/notmuch-multipart/alternative-discouraged)

    ;; display images
    (setq notmuch-show-text/html-blocked-images nil)

    (require 'shr)
    ;; shr-tag-img will ignore images with size=1
    (my/define-advice shr-tag-img (:before (dom &rest _) fix-github-email-beacon-img)
      (when (string-match-p "/notifications/beacon/" (dom-attr dom 'src))
        (dom-set-attribute dom 'width "2")
        (dom-set-attribute dom 'height "2")))

    (defun my/notmuch-search-mark-read ()
      (interactive)
      (notmuch-search-tag '("-unread"))
      (notmuch-search-next-thread))

    (defun my/notmuch-search-mark-unread ()
      (interactive)
      (notmuch-search-tag '("+unread"))
      (notmuch-search-next-thread))

    (evil-define-key '(normal motion) notmuch-search-mode-map
      (kbd "a") #'ignore  ;; originally archive
      (kbd "A") #'notmuch-search-archive-thread
      (kbd "R") #'my/notmuch-search-mark-read
      ;; (kbd "r") #'my/notmuch-search-mark-read
      (kbd "U") #'my/notmuch-search-mark-unread
      ;; (kbd "u") #'my/notmuch-search-mark-unread
      (kbd "x") #'notmuch-refresh-this-buffer)

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

    (defun my/-notmuch-move-to-path (query target-dir)
      (let* ((dir (expand-file-name target-dir "~/Maildir/"))
             (cmd (concat "notmuch search --format=text0 --output=files "
                          (shell-quote-argument query)
                          " | xargs -0 -n 1 -I % "
                          "mv % " dir "/")))
        (let ((inhibit-message t))
          (message "executing: %s" cmd))
        (call-process-shell-command cmd)))

    (defun my/notmuch-move-path-based-on-tag ()
      "Used as `notmuch-before-tag-hook', move emails between paths based on tag changes."
      (when (member "-inbox" tag-changes)
        (my/-notmuch-move-to-path query "archive"))
      (when (member "+inbox" tag-changes)
        (my/-notmuch-move-to-path query "cur")))

    (add-hook 'notmuch-before-tag-hook #'my/notmuch-move-path-based-on-tag)

    ;; browse url (written by claude)
    (defun my/notmuch-find-text-plain-content (parts)
      "Recursively find and return content from text/plain parts."
      (let (content)
        (dolist (part parts)
          (let ((content-type (plist-get part :content-type))
                (part-content (plist-get part :content)))
            (cond
             ;; If this is text/plain, collect its content
             ((string= content-type "text/plain")
              (when (stringp part-content)
                (setq content (concat content part-content "\n"))))
             ;; If content is a list (multipart), recurse
             ((listp part-content)
              (let ((sub-content (my/notmuch-find-text-plain-content part-content)))
                (when sub-content
                  (setq content (concat content sub-content))))))))
        content))

    (defun my/notmuch-extract-urls-from-current-message ()
      "Extract all URLs from the text/plain part of the current notmuch message."
      (interactive)
      (let ((msg (notmuch-show-get-message-properties))
            urls)
        (when msg
          (let* ((body (plist-get msg :body))
                 (text-content (my/notmuch-find-text-plain-content body)))
            (when text-content
              (with-temp-buffer
                (insert text-content)
                (goto-char (point-min))
                (while (re-search-forward
                        "https?://[[:alnum:].-]+\\(?:[/?#][^[:space:]]*\\)?"
                        nil t)
                  (push (match-string 0) urls))))))
        (delete-dups (nreverse urls))))

    (defun my/notmuch-browse-url ()
      "Extract URLs from current notmuch message and browse selected URL."
      (interactive)
      (let ((urls (my/notmuch-extract-urls-from-current-message)))
        (if (null urls)
            (message "No URLs found in current message")
          (let ((selected-url (completing-read "Browse URL: " urls nil t)))
            (when selected-url
              (browse-url selected-url)
              (message "Opening: %s" selected-url))))))

    (evil-define-key '(normal motion) notmuch-show-mode-map
      (kbd "g l") #'my/notmuch-browse-url)

    )

  )  ;; }}}

(progn  ;; UI {{{
  (use-package pixel-scroll
    :demand t
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

(progn  ;; doc viewer
  (use-package pdf-tools
    :custom
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-09/msg00972.html
    (pdf-annot-tweak-tooltips nil)
    :init
    (require 'pdf-loader)
    (pdf-loader-install)
    :config
    (evil-define-key 'normal pdf-view-mode-map
      (kbd "C-/") #'pdf-occur
      ;; clear highlight. "/" maps to isearch-forward for pdf-viewer
      (kbd "C-l") #'isearch-exit)
    (add-to-list 'pdf-tools-enabled-modes 'pdf-view-themed-minor-mode)

    (require 'pdf-isearch)
    ;; https://github.com/vedang/pdf-tools/issues/162
    (my/define-advice pdf-isearch-hl-matches (:filter-args (args) fix-isearch-highlight)
      ;; args: current matches &optional occur-hack-p
      (list (car args)
            (cadr args)
            (or (caddr args)
                (memq this-command '(isearch-repeat-forward isearch-repeat-backward isearch-printing-char)))))

    ))

(progn  ;; Misc {{{
  (custom-set-variables
   '(term-buffer-maximum-size 20480)

   ;; it was reversed... (wtf?)
   ;; https://mail.gnu.org/archive/html/emacs-devel/2019-03/msg00002.html
   '(tabulated-list-gui-sort-indicator-asc ?▲)
   '(tabulated-list-gui-sort-indicator-desc ?▼)

   ;; System UI related
   '(use-dialog-box nil)
   `(use-system-tooltips ,(my/macos-p))
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

  (when (my/macos-p)
    (defun my/macos-refocus-frame ()
      (select-frame-set-input-focus (selected-frame)))
    (add-hook 'focus-in-hook #'my/macos-refocus-frame))

  )  ;;; }}}

(progn  ;; Load custom.el, enable customization UI  {{{
  ;; set custom-file to another file, but only load SOME of them
  ;; steps to change variable using Customization UI: apply and save, review it, put it in this file.
  (setq custom-file "~/.emacs.d/custom.el")

  ;; Load some whitelisted variables from custom.el
  (setq my/allowed-custom-variables
        '(safe-local-variable-values custom-safe-themes codeium/metadata/api_key))

  (when-let* ((_ (file-exists-p custom-file))
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
