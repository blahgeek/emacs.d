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

(progn  ;; Package Manager: straight, use-package {{{
  (setq straight-use-package-by-default t
        straight-vc-git-default-clone-depth 20
        straight-check-for-modifications '(check-on-save find-when-checking)
        vc-follow-symlinks t)

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (setq use-package-verbose t
        ;; always defer. this is important
        use-package-always-defer t)
  (straight-use-package 'use-package)

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

  (defvar prog-mode-local-only-hook nil
    "Custom hook for prog-mode, but local only (not triggered for TRAMP file)")
  (defun my/trigger-prog-mode-local-only-hook ()
    "Trigger `prog-mode-local-only-hook' on prog-mode, if it's a local buffer."
    (unless (and (buffer-file-name)
                 (file-remote-p (buffer-file-name)))
      (run-hooks 'prog-mode-local-only-hook)))

  (add-hook 'prog-mode-hook #'my/trigger-prog-mode-local-only-hook)
  )  ;; }}}

(progn  ;; pragmata ligatures and icons {{{
  (use-package ligature
    :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
    :config
    (ligature-set-ligatures
     'prog-mode '("!!" "!=" "!!!" "!==" "&&" "***" "*=" "*/" "++" "+=" "--" "-="
                  "->" ".." "..." "/*" "//" "/>" "///" "/**" ":::" "::" ":=" "<-"
                  "<<" "<=" "<=>" "==" "===" "=>" ">=" ">>" "??" "\\\\" "|=" "||"
                  "[[" "]]"))
    :hook (prog-mode . ligature-mode))

  ;; pragmata major mode icons
  (delight '((dired-mode "\xe5fe" :major)
             (python-mode "\xe606" :major)
             (js-mode "\xe60c" :major)
             (sh-mode "\xe614" :major)
             (c++-mode "\xe61d" :major)
             (c-mode "\xe61e" :major)
             (go-mode "\xe626" :major)
             (web-mode "\xe796" :major)
             (lua-mode "\xe620" :major)
             (typescript-mode "\xe628" :major)
             (vimrc-mode "\xe62b" :major)
             (html-mode "\xe736" :major)
             (java-mode "\xe738" :major)
             (ruby-mode "\xe739" :major)
             (markdown-mode "\xe73e" :major)
             (haskell-mode "\xe777" :major)
             ;; rust-mode e7a8
             (vterm-mode "\xe795" :major)
             (dockerfile-mode "\xe7b0" :major)))
  ;; see delight.el
  (advice-add 'c-update-modeline :override #'ignore)

  ;; `truncate-string-ellipsis' returns "…" (\u2026) by default
  ;; this char should be double-char-width
  ;; but it's single-char-width in my font (because my customization to fix a bug),
  ;; which would make tables unaliged
  ;; https://github.com/fabrizioschiavi/pragmatapro/issues/217
  (setq truncate-string-ellipsis "...")
  )  ;; }}}

(progn  ;; EVIL & general keybindings {{{
  (use-package undo-fu)

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
          evil-undo-system 'undo-fu
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
    ;; 2. do not set cursor position, only focus window
    (evil-define-key 'motion 'global
      [down-mouse-1] nil)
    (evil-define-key nil 'global
      [down-mouse-1] nil
      [drag-mouse-1] nil
      ;; control-mouse scroll
      [C-mouse-4] nil
      [C-mouse-5] nil
      [mouse-1] #'mouse-select-window)
    ;; other general keybindings
    (evil-define-key nil 'global
      (kbd "C-S-v") #'yank)
    (evil-define-key 'normal 'global
      (kbd "C-l") #'evil-ex-nohighlight
      (kbd "Q") "@q"
      (kbd "C-:") #'execute-extended-command
      (kbd "U") #'evil-redo))

  (use-package evil-collection
    :demand t
    :after evil
    :config
    ;; remove keybindings for some modes. let's do them on our own
    (mapc (lambda (x) (setq evil-collection-mode-list (delete x evil-collection-mode-list)))
          '(vterm company))
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
    :custom
    (evil-snipe-repeat-keys nil)
    (evil-snipe-scope 'buffer)
    :delight evil-snipe-local-mode
    :config
    ;; global mode
    (evil-snipe-mode))

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

(progn  ;; Theme {{{
  (use-package solarized-theme
    :demand t
    :custom
    (solarized-use-variable-pitch nil)
    (solarized-use-more-italic t)
    ;; (solarized-emphasize-indicators nil)  ;; this will remove the flycheck fringe background
    :config
    (defvar my/use-light-theme t)
    (defvar my/after-switch-theme-hook nil
      "Hook run after switching theme.")

    (defun my/load-solarized (light-theme)
      "Load solarized theme, light if LIGHT-THEME is true."
      ;; Fix term-color-black:
      ;; by default, term-color-black is base02 (see solarized-faces.el)
      ;; but that's a background color (very light in solarized-light)
      ;; in xonsh, this color is used for displaying aborted commands and suggestions,
      ;; which should be a "comment"-like foreground color, which is base01
      (let ((name (if light-theme 'solarized-light 'solarized-dark))
            (base01 (if light-theme "#93a1a1" "#586e75")))
        (load-theme name t)
        (custom-set-faces
         `(term-color-black ((t (:foreground ,base01 :background ,base01)))))))

    (my/load-solarized t)
    (defun my/switch-light-dark-theme ()
      "Switch solarized light/dark theme."
      (interactive)
      (setq my/use-light-theme (not my/use-light-theme))
      (my/load-solarized my/use-light-theme)
      (run-hooks 'my/after-switch-theme-hook))
    (define-key global-map
      (kbd "C-x -") #'my/switch-light-dark-theme))

  (setq-default mode-line-format
                (delete '(vc-mode vc-mode) mode-line-format))
  (custom-set-faces
   '(fixed-pitch ((t (:family nil :inherit default))))
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
  )  ;; }}}

(progn  ;; Some essential utils {{{
  (use-package switch-buffer-functions
    :demand t)
  (use-package exec-path-from-shell
    :when (my/macos-p)
    :demand t
    :config
    (exec-path-from-shell-initialize))
  (use-package add-node-modules-path
    :hook js-mode)
 (use-package fringe-scale
   :straight (emacs-fringe-scale :type git :host github :repo "blahgeek/emacs-fringe-scale")
   :demand t
   :unless (my/macos-p)
   :init (setq fringe-scale-width my/gui-fringe-size)
   :config (fringe-scale-setup))

  (use-package which-key
    :demand t
    :delight which-key-mode
    :custom (which-key-ellipsis "..")  ;; see `truncate-string-ellipsis'
    :config (which-key-mode t))

  )  ;; }}}


(progn  ;; minibuffer completion {{{

  (use-package recentf
    :straight nil
    :config
    (defun my/recentf-keep-predicate (file)
      (and (not (file-remote-p file))
           (file-readable-p file)))
    (setq recentf-keep '(my/recentf-keep-predicate)))

  (use-package orderless
    :init
    (defun my/orderless-set-local-completion-style ()
      (setq-local completion-styles '(orderless)
                  completion-category-defaults nil))
    ;; only set for minibuffer, do not affect company-capf
    :hook (minibuffer-setup . my/orderless-set-local-completion-style)
    :config
    ;; https://github.com/minad/vertico/blob/0831da48fe75a173a27eb1ff2837777c80f0a2f4/vertico.el#L296
    ;; https://github.com/minad/vertico/issues/27#issuecomment-1057924544
    (let ((orig-fn (symbol-function 'orderless-highlight-matches)))
      (defun my/orderless-reset-function ()
        (fset 'orderless-highlight-matches orig-fn))
      (add-hook 'minibuffer-setup-hook #'my/orderless-reset-function)))

  (use-package vertico
    :demand t
    :straight (vertico :fetcher github :repo "minad/vertico"
                       :includes (vertico-directory)
                       :files (:defaults "extensions/*.el"))
    :custom
    (vertico-sort-function nil)
    :config
    (vertico-mode)
    (define-key vertico-map (kbd "C-j") (kbd "C-n"))
    (define-key vertico-map (kbd "C-k") (kbd "C-p")))

  (use-package vertico-directory
    :after vertico
    :straight nil
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
    :init
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

    (evil-define-key '(insert emacs normal motion) 'global
      (kbd "C-t") #'my/consult-buffer-vterm-only
      (kbd "C-r") #'consult-buffer)
    (evil-define-key 'normal 'global
      (kbd "g s") #'consult-imenu  ;; LSP would integrate with imenu to provide file symbols
      (kbd "g S") #'consult-imenu-multi
      (kbd "C-/") #'consult-line
      (kbd "C-?") #'my/consult-ripgrep-ask-dir)
    :commands (my/consult-buffer-vterm-only
               my/consult-ripgrep-ask-dir)
    :config
    (recentf-mode 1)

    ;; consult buffers
    (delete 'consult--source-bookmark consult-buffer-sources)
    (delete 'consult--source-buffer consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'my/consult--source-buffer)

    (defun my/consult-buffer-vterm-only ()
      (interactive)
      (let ((consult-buffer-sources '(my/consult--source-vterm-buffer)))
        (consult-buffer)))

    (defun my/consult-ripgrep-ask-dir ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively #'consult-ripgrep)))

    ;; xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (use-package embark
    :custom
    (embark-prompter 'embark-completing-read-prompter)
    (embark-indicators '(embark-minimal-indicator embark-highlight-indicator))
    :init
    (evil-define-key '(normal motion emacs) 'global
      (kbd "C-.") #'embark-act)
    ;; evil-define-key does not work on minibuffer
    :bind (("C-." . embark-act))
    :config
    (define-key embark-url-map
      "B" #'browse-url-with-browser-kind))

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
   ;; scroll
   '(scroll-conservatively 101)
   '(hscroll-step 1)
   '(scroll-margin 2)
   '(scroll-step 1)
   ;; editing
   '(truncate-lines t)
   '(tab-always-indent nil)
   '(tab-width 4))

  (setq redisplay-skip-fontification-on-input t)

  (defun my/shorten-auto-save-file-name (&rest args)
    "Shorten filename using hash function so that it will not be too long."
    (let ((buffer-file-name
           (when buffer-file-name (sha1 buffer-file-name))))
      (apply args)))
  (advice-add 'make-auto-save-file-name :around
              #'my/shorten-auto-save-file-name)

  )  ;; }}}

(progn  ;; Builtin editing-related packages: whitespace, hl-line, ... {{{
  (use-package outline
    :straight nil
    :delight outline-minor-mode)

  (use-package whitespace
    :straight nil
    :hook (prog-mode . whitespace-mode)
    :delight whitespace-mode
    :custom (whitespace-style '(face trailing indentation space-after-tab space-before-tab tab-mark))
    :custom-face (whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :inherit whitespace-space)))))

  (use-package hl-line
    :straight nil
    :hook
    (prog-mode . hl-line-mode)
    (tabulated-list-mode . hl-line-mode))

  (use-package display-line-numbers
    :straight nil
    :hook (prog-mode . display-line-numbers-mode))

  (use-package elec-pair
    :straight nil
    :init (setq electric-pair-skip-whitespace nil)
    :hook (prog-mode . electric-pair-local-mode))

  (use-package paren
    :straight nil
    :demand t
    :init (setq show-paren-when-point-inside-paren t)
    :config (show-paren-mode t))

  (use-package autorevert
    :straight nil
    :demand t
    :delight auto-revert-mode
    :config
    (global-auto-revert-mode t))

  (use-package eldoc
    :straight nil
    ;; delight
    :init (setq eldoc-minor-mode-string nil))

  (use-package tramp
    :straight nil
    :config (setq vc-ignore-dir-regexp
                  (format "\\(%s\\)\\|\\(%s\\)"
                          locate-dominating-stop-dir-regexp
                          tramp-file-name-regexp)))

  ;; delight some common minor modes
  (delight '((abbrev-mode nil "abbrev")))

  ) ;; }}}

(progn  ;; Editing-related packages: indent, git-gutter, .. {{{
  ;; git-gutter is better than diff-hl
  (use-package git-gutter-fringe
    :delight git-gutter-mode
    :init
    ;; by default, git-gutter-mode will autoload "git-gutter" without fringe
    (autoload 'git-gutter-mode "git-gutter-fringe" nil t)
    :hook (prog-mode-local-only . git-gutter-mode))

  (use-package rainbow-mode
    :hook ((html-mode web-mode css-mode) . rainbow-mode))

  (use-package hl-todo
    :delight hl-todo-mode
    :hook (prog-mode . hl-todo-mode))

  (use-package dtrt-indent
    :delight dtrt-indent-mode
    :hook (prog-mode . dtrt-indent-mode)
    :config
    (add-to-list 'dtrt-indent-hook-mapping-list
                 '(cmake-mode default cmake-tab-width))
    (add-to-list 'dtrt-indent-hook-mapping-list
                 '(web-mode javascript web-mode-code-indent-offset)))
  )  ;; }}}

(use-package autoinsert  ;; Auto-insert File headers {{{
  :straight nil
  :delight auto-insert-mode
  ;; hook `auto-insert', not `auto-insert-mode', because the latter is a global mode
  :hook ((c++-mode . auto-insert)
         (c-mode . auto-insert)
         (python-mode . auto-insert)
         (protobuf-mode . auto-insert))
  :config
  (define-auto-insert
    `(,(rx "." (or "h" "hpp" "hh") eos) . "C++ header")
    '(nil
      "#pragma once" \n \n))
  (define-auto-insert
    `(,(rx ".py" eos) . "Python header")
    '(nil
      "#!/usr/bin/env python3" \n "# -*- coding: utf-8 -*-" \n \n))
  (define-auto-insert
    `(,(rx ".proto" eos) . "Protobuf header")
    '("Package: "
      "syntax = \"proto2\";" \n \n "package " str ";" \n \n _))
  )  ;; }}}

(progn  ;; Filetypes (Major modes)  {{{
  (defun my/ensure-prog-mode ()
    "Run `prog-mode-hook' and related settings.
Useful for modes that does not derive from `prog-mode'."
    (unless (derived-mode-p 'prog-mode)
      (setq-local require-final-newline t)
      (run-hooks 'prog-mode-hook)))

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
    :straight (bazel :type git :host github :repo "bazelbuild/emacs-bazel-mode")
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

  (use-package typescript-mode)

  ;; Use web-mode for .tsx (react typescript). This is the only way
  (use-package web-mode
    :custom
    (web-mode-enable-css-colorization nil)
    (web-mode-enable-auto-pairing nil)
    (web-mode-enable-auto-quoting nil)
    (web-mode-enable-heredoc-fontification nil)
    (web-mode-enable-sexp-functions nil)
    (web-mode-comment-formats '(("java" . "//")
                                ("javascript" . "//")
                                ("typescript" . "//")
                                ("php" . "/*")
                                ("css" . "/*")))
    :mode (rx ".tsx" eos))

  (use-package lua-mode)

  (use-package haskell-mode)

  (use-package jsonnet-mode)

  (use-package dockerfile-mode)

  (use-package bpftrace-mode)

  (use-package cuda-mode
    :config (add-hook 'cuda-mode-hook #'my/ensure-prog-mode))

  (add-to-list 'auto-mode-alist `(,(rx ".mm" eos) . objc-mode))

  (setq python-prettify-symbols-alist '())

  ;; CC mode
  (use-package google-c-style
    :straight (google-c-style :fetcher github :repo "google/styleguide" :branch "gh-pages")
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

(progn  ;; Tree-sitter {{{
  (use-package tree-sitter
    :hook (prog-mode . turn-on-tree-sitter-mode)
    :delight " TS")

  (use-package tree-sitter-langs
    :demand t
    :after tree-sitter)

  (use-package evil-textobj-tree-sitter
    :demand t
    :after evil
    :config
    (defmacro my/define-treesitter-textobj (inner-or-outer ch item)
      "Define evil treesitter text-obj."
      `(define-key
         ,(intern (concat "evil-" (symbol-name inner-or-outer) "-text-objects-map"))
         ,ch (evil-textobj-tree-sitter-get-textobj ,item)))
    (defmacro my/define-treesitter-goto (key item &optional previous end)
      "Define evil treesitter goto-key."
      `(define-key evil-normal-state-map
         (kbd ,key)
         (lambda () (interactive)
           (evil-textobj-tree-sitter-goto-textobj ,item, previous ,end))))

    (my/define-treesitter-textobj inner "a" "parameter.inner")
    (my/define-treesitter-textobj outer "a" "parameter.outer")
    (my/define-treesitter-textobj inner "b" "block.inner")
    (my/define-treesitter-textobj outer "b" "block.outer")
    (my/define-treesitter-textobj inner "f" "function.inner")
    (my/define-treesitter-textobj outer "f" "function.outer")
    (my/define-treesitter-textobj outer "#" "comment.outer")
    (my/define-treesitter-textobj inner "#" "comment.outer")  ;; no comment.inner

    (my/define-treesitter-goto "]f" "function.outer")
    (my/define-treesitter-goto "[f" "function.outer" t))
  )  ;;; }}}

(progn  ;; ORG mode {{{
  (use-package org
    :straight nil
    :init
    (setq org-directory "~/Notes"
          org-agenda-files '("~/Notes/gtd/")
          org-capture-templates '(("i" "Inbox" entry
                                   (file+olp "gtd/inbox.org" "Inbox")
                                   "* %i%? \n ADDED: %U\n")
                                  ("p" "Pony Inbox" entry
                                   (file+olp "gtd/pony.org" "Pony" "Inbox")
                                   "* %i%? \n ADDED: %U\n"))
          org-refile-use-outline-path t
          org-outline-path-complete-in-steps nil)

    :mode ((rx ".org" eos) . org-mode)
    :bind (("C-S-o l" . org-store-link)
           ("C-S-o a" . org-agenda)
           ("C-S-o c" . org-capture)))

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
    ;; :straight (vterm :fetcher github :repo "blahgeek/emacs-libvterm" :branch "blah")
    :demand t
    :init
    (setq
     vterm-kill-buffer-on-exit t
     vterm-max-scrollback 10000
     vterm-buffer-name-string "%%vterm %s"   ;; see my/consult--source-vterm-buffer
     vterm-shell (or (executable-find "xonsh") shell-file-name))
    :config
    (defun my/vterm-set-pwd (path)
      "Set default-directory"
      ;; only set if we're still in vterm buffer
      ;; to workaround the prompt after find-file
      (if (eq major-mode 'vterm-mode)
          (setq default-directory path)))
    (add-to-list 'vterm-eval-cmds '("set-pwd" my/vterm-set-pwd))
    (add-to-list 'vterm-eval-cmds '("man" man))
    (add-to-list 'vterm-eval-cmds '("magit-status" magit-status))
    (evil-set-initial-state 'vterm-mode 'insert)
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
    (evil-define-key 'normal vterm-mode-map
      (kbd "C-S-v") 'vterm-yank
      ;; Do not allow insertion commands in normal mode. Only allow "a"
      ;; [remap evil-append] #'ignore
      ;; [remap evil-append-line] #'ignore
      [remap evil-insert] #'ignore
      [remap evil-insert-line] #'ignore
      [remap evil-change] #'ignore
      [remap evil-change-line] #'ignore
      [remap evil-substitute] #'ignore
      [remap evil-change-whole-line] #'ignore
      [remap evil-delete] #'ignore
      [remap evil-delete-line] #'ignore
      [remap evil-delete-char] #'ignore
      [remap evil-delete-backward-char] #'ignore
      [remap evil-replace] #'ignore
      [remap evil-replace-state] #'ignore
      [remap evil-open-below] #'ignore
      [remap evil-open-above] #'ignore
      [remap evil-paste-after] #'ignore
      [remap evil-paste-before] #'ignore
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
      (setq evil-normal-state-cursor '(box "red")
            evil-insert-state-cursor `(box ,(face-attribute 'default :foreground)))
      ;; buffer-local evil hook, always reset cursor after entering insert state
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
            (newbuf (get-buffer-create (format "*vterm clone - %s*" (buffer-name)))))
        (with-current-buffer newbuf
          (insert content))
        (switch-to-buffer-other-window newbuf)))
    (evil-ex-define-cmd "vterm-clone" #'my/vterm-clone-to-new-buffer)

    ;; both perspective.el and emacs server itself will call initial-buffer-choice
    ;; so setting initial-buffer-choice to 'vterm will end up creating two terms
    (setq initial-buffer-choice
          (lambda ()
            (let ((buf (frame-parameter nil 'my--initial-vterm-buffer)))
              (unless buf
                ;; emacs deamon will call initial-buffer-choice without visible frame
                (setq buf (if (frame-parameter nil 'visibility)
                              (my/with-editor-vterm)
                            (get-buffer-create "*scratch*")))
                (set-frame-parameter nil 'my--initial-vterm-buffer buf))
              buf))))

  (defun my/vterm-process-kill-buffer-query-function ()
    (let ((process (get-buffer-process (current-buffer))))
      (or (not process)
          (not (eq major-mode 'vterm-mode))
          (not (memq (process-status process) '(run stop open listen)))
          ;; does not have any subprocess
          (not (member (process-id process)
                       (mapcar (lambda (p) (alist-get 'ppid (process-attributes p)))
                               (list-system-processes))))
          (yes-or-no-p (format "VTerm %S has a running subprocess; kill it? "
                               (buffer-name (current-buffer)))))))
  (add-hook 'kill-buffer-query-functions #'my/vterm-process-kill-buffer-query-function)

  )  ;; }}}

(progn  ;; Project / Window management {{{
  (use-package projectile
    :init
    ;; Set "projectile-project-name" to override the name
    (defun my/projectile-mode-line ()
      "Modified version of projectile-default-mode-line"
      (format " @%s" (or (projectile-project-name) "-")))
    (setq projectile-completion-system 'default
          projectile-enable-caching t
          ;; https://github.com/bbatsov/projectile/issues/1749
          projectile-generic-command "fd . -0 --type f --color=never --strip-cwd-prefix"
          projectile-switch-project-action #'projectile-dired
          projectile-mode-line-function #'my/projectile-mode-line
          ;; The following line is actually unused anymore
          projectile-mode-line-prefix " Proj")
    (autoload 'projectile-command-map "projectile" nil nil 'keymap)
    (evil-define-key '(normal motion emacs) 'global (kbd "C-p") 'projectile-command-map)
    :hook (prog-mode-local-only . projectile-mode)
    :commands projectile-project-root
    :config
    (projectile-mode t)
    (define-key projectile-command-map "F" 'projectile-find-file-other-window)
    (define-key projectile-command-map "h" 'projectile-find-other-file)
    (define-key projectile-command-map "H" 'projectile-find-other-file-other-window)

    (defadvice projectile-project-root (around ignore-remote first activate)
      (unless (file-remote-p default-directory)
        ad-do-it))

    ;; Bridge projectile and project together so packages that depend on project
    ;; like eglot work
    (defun my/projectile-project-find-function (dir)
      (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root))))
    (with-eval-after-load 'project
      (add-to-list 'project-find-functions #'my/projectile-project-find-function)))

  (use-package winner
    :demand t
    :straight nil
    :delight winner-mode
    :config
    (winner-mode t)
    (evil-define-key '(normal motion emacs) 'global
      (kbd "C-w u") 'winner-undo
      (kbd "C-w x") 'kill-this-buffer))
  )  ;; }}}

(progn  ;; Snippets, completion {{{
  (use-package yasnippet
    :hook ((prog-mode . yas-minor-mode)
           (pr-review-input-mode . yas-minor-mode))
    :delight yas-minor-mode
    :config
    (yas-reload-all))

  ;; (use-package yasnippet-snippets)

  (use-package company
    :init
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0  ;; default is 0.2
          ;; NOTE: revert this if it's slow
          company-search-regexp-function 'company-search-flex-regexp
          company-tooltip-align-annotations t
          ;; show single candidate as tooltip
          company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
          company-backends '(company-files
                             (company-yasnippet :separate company-capf)
                             (company-dabbrev-code company-gtags company-etags company-keywords)
                             company-dabbrev
                             ;; company-capf will never be used at this position
                             ;; but adding it here can prevent lsp-completion.el to add it to the beginning of the list
                             company-capf))
    :delight company-mode
    :hook ((prog-mode . company-mode)
           (pr-review-input-mode . company-mode)
           (comint-mode . company-mode))
    :config
    ;; TODO: needs more improvement
    (evil-define-key 'insert comint-mode-map
      (kbd "<tab>") #'company-complete)
    ;; company-box is slow
    ;; (use-package company-box  ;; this is better. does not hide line number while showing completions
    ;;   :init (setq company-box-show-single-candidate t
    ;;               company-box-doc-delay 1
    ;;               company-box-icons-alist 'company-box-icons-all-the-icons)
    ;;   :hook (company-mode . company-box-mode)
    ;;   :delight company-box-mode)
    (evil-define-key nil company-active-map
      (kbd "C-n") 'company-select-next-or-abort
      (kbd "C-p") 'company-select-previous-or-abort
      (kbd "C-j") 'company-select-next-or-abort
      (kbd "C-k") 'company-select-previous-or-abort
      (kbd "<tab>") 'company-complete-selection
      ;; (kbd "<tab>") (lambda () (interactive) (company-complete-common-or-cycle 1))
      (kbd "RET") nil
      (kbd "<return>") nil
      (kbd "C-h") nil
      ;; the completion popup will not disappear while working with lsp and capf
      ;; https://github.com/emacs-lsp/lsp-mode/issues/1447
      (kbd "<escape>") (lambda () (interactive) (company-abort) (evil-normal-state)))
    (evil-define-key nil company-search-map
      (kbd "C-j") 'company-select-next-or-abort
      (kbd "C-k") 'company-select-previous-or-abort
      (kbd "<escape>") 'company-search-abort)
    ;; (company-tng-configure-default)
    )

  (use-package company-emoji
    :after company
    :custom
    (company-emoji-insert-unicode nil))

  )  ;; }}}

(progn  ;; Flycheck  {{{
  (use-package flycheck
    :custom
    (flycheck-python-pylint-executable "pylint")
    (flycheck-emacs-lisp-load-path 'inherit)
    :hook (prog-mode-local-only . flycheck-mode)
    :config

    ;; https://github.com/flycheck/flycheck/issues/1762
    (defvar-local my/lsp-next-checkers nil "Custom :next-checkers for lsp checker")
    (defun my/flycheck-checker-get (fn checker property)
      "Custom flycheck-checker-get to use my/lsp-next-checkers"
      (if (and (equal checker 'lsp) (equal property 'next-checkers))
          my/lsp-next-checkers
        (funcall fn checker property)))
    (advice-add 'flycheck-checker-get :around #'my/flycheck-checker-get)

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
    (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (use-package consult-flycheck
    :init (evil-define-key 'normal 'global
            (kbd "g !") #'consult-flycheck))

  (use-package flycheck-google-cpplint
    :after flycheck
    :custom (flycheck-c/c++-googlelint-executable "cpplint")
    :demand t)

  (use-package flycheck-package
    :after flycheck)

  (use-package flycheck-posframe
    :straight (flycheck-posframe :fork (:repo "blahgeek/flycheck-posframe"))
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config
    ;; https://github.com/alexmurray/flycheck-posframe/issues/25
    ;; evil-normal-state-entry-hook: hide posframe on ESC in normal state
    (setq flycheck-posframe-hide-posframe-hooks '(evil-normal-state-entry-hook)
          flycheck-posframe-timeout 0.0
          flycheck-display-errors-delay 0.2)
    (add-hook 'flycheck-posframe-inhibit-functions
              ;; only show in normal state
              (lambda () (not (equal evil-state 'normal))))

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
    :straight (flymake-posframe :type git :host github :repo "ladicle/flymake-posframe")
    :hook (flymake-mode . flymake-posframe-mode))
  )  ;; }}}

(progn  ;; LSP-mode  {{{
  (use-package lsp-mode
    :init
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
     lsp-modeline-diagnostics-enable nil)
    (evil-define-minor-mode-key 'normal 'lsp-mode
      (kbd "g r") #'lsp-find-references
      (kbd "g x") #'lsp-execute-code-action)
    (evil-define-minor-mode-key '(normal visual motion) 'lsp-mode
      (kbd "+") #'lsp-format-region)
    (evil-define-minor-mode-key 'insert 'lsp-mode
      (kbd "C-l") #'lsp-signature-activate)
    (defun my/maybe-start-lsp ()
      "Run `lsp-deferred' if the following condition matches:
1. major modes not blacklisted;
2. folder is already imported.
Otherwise, I should run `lsp' manually."
      (when (and (not (memq major-mode '(xonsh-mode bpftrace-mode)))
                 (lsp-find-session-folder (lsp-session) (buffer-file-name)))
        (lsp-deferred)))
    :hook ((c++-mode . my/maybe-start-lsp)
           (c-mode . my/maybe-start-lsp)
           (objc-mode . my/maybe-start-lsp)
           (python-mode . my/maybe-start-lsp)
           (go-mode . my/maybe-start-lsp)
           (haskell-mode . my/maybe-start-lsp)
           (haskell-literate-mode . my/maybe-start-lsp)
           (js-mode . my/maybe-start-lsp)
           (typescript-mode . my/maybe-start-lsp)
           (web-mode . my/maybe-start-lsp)  ;; .tsx
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
    (defun my/lsp-ignore-multi-root (&rest _args)
      "Ignore multi-root while starting lsp."
      (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))
    (advice-add 'lsp :before #'my/lsp-ignore-multi-root)

    ;; try to fix memory leak
    (defun my/lsp-client-clear-leak-handlers (lsp-client)
      "Clear leaking handlers in LSP-CLIENT."
      (let ((response-handlers (lsp--client-response-handlers lsp-client))
            to-delete-keys)
        (maphash (lambda (key value)
                   (when (> (time-convert (time-since (nth 3 value)) 'integer)
                            (* 2 lsp-response-timeout))
                     (push key to-delete-keys)))
                 response-handlers)
        (when to-delete-keys
          (message "Deleting %d handlers in %s lsp-client..."
                   (length to-delete-keys)
                   (lsp--client-server-id lsp-client))
          (mapc (lambda (k) (remhash k response-handlers))
                to-delete-keys))))
    (defun my/lsp-clear-leak ()
      "Clear all leaks"
      (maphash (lambda (_ client)
                 (my/lsp-client-clear-leak-handlers client))
               lsp-clients))
    (setq my/lsp-clear-leak-timer
          (run-with-timer 5 5 #'my/lsp-clear-leak)))

  (use-package lsp-java
    :demand t
    :init (setq lsp-java-configuration-maven-user-settings (expand-file-name "~/.m2/settings.xml"))
    :after lsp-mode)

  (use-package lsp-pyright
    :demand t
    :after lsp-mode)

  (use-package lsp-haskell
    :demand t
    :after lsp-mode
    :custom (lsp-pyright-multi-root nil))

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
    :hook ((c++-mode . eglot-ensure))
    :config (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))
  )  ;; }}}

(progn  ;; External integration {{{
  (use-package magit
    :init
    (evil-define-key 'normal 'global (kbd "C-s") 'magit)
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

    (let ((gitconfig-fsmonitor (expand-file-name "~/.gitconfig_fsmonitor")))
      (when (file-exists-p gitconfig-fsmonitor)
        (setq magit-git-global-arguments (append `("-c" ,(concat "include.path=" gitconfig-fsmonitor))
                                                 magit-git-global-arguments)))))

  (use-package pr-review
    :straight (pr-review :local-repo "~/Code/emacs-pr-review"
                         :files (:defaults "graphql"))
    :init
    (evil-ex-define-cmd "prr" #'pr-review)
    (evil-ex-define-cmd "prs" #'pr-review-search-open)
    (evil-ex-define-cmd "prn" #'pr-review-notification)
    (add-to-list 'browse-url-default-handlers
                 '(pr-review-url-parse . pr-review-open-url))
    :config
    (defun my/enable-company-emoji-buffer-local ()
      (set (make-local-variable 'company-backends)
           '(company-emoji company-yasnippet)))
    (add-hook 'pr-review-input-mode-hook #'my/enable-company-emoji-buffer-local))

  (use-package git-link
    :custom
    (git-link-open-in-browser t))

  (use-package ag
    :init
    (setq ag-highlight-search t)
    (evil-ex-define-cmd "ag" #'ag)
    (bind-keys :prefix "C-c a"
               :prefix-map ag-prefix-map
               ("g" . ag)
               ("f" . ag-files)
               ("r" . ag-regexp)
               ("d" . ag-dired)
               ("s" . ag-dired-regexp)))

  (use-package wgrep-ag
    :after ag
    :demand t
    :init (setq wgrep-auto-save-buffer t))

  (use-package fcitx
    :demand t
    :init
    (unless (my/macos-p)
      (setq fcitx-use-dbus t))
    :config
    (fcitx-evil-turn-on))

  (use-package dumb-jump
    :init
    (setq dumb-jump-selector 'completing-read
          dumb-jump-default-project "~/Code/")
    (evil-define-key 'normal 'global
      (kbd "g]") #'dumb-jump-go
      (kbd "g c-]") #'dumb-jump-go-other-window)
    :config
    (advice-add 'dumb-jump-go :before (lambda (&rest _) (evil-set-jump)))
    (defun my/dumb-jump-get-project-root (filepath)
      "Get project root for dumb jump using projectile."
      (s-chop-suffix "/" (expand-file-name
                          (or (projectile-project-root filepath)
                              dumb-jump-default-project))))
    (advice-add 'dumb-jump-get-project-root :override #'my/dumb-jump-get-project-root))

  (use-package sudo-edit
    :init (evil-ex-define-cmd "su[do]" #'sudo-edit)
    :config
    (sudo-edit-indicator-mode))

  (use-package server
    :straight nil
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
    (unless (my/macos-p)
      (when (fboundp 'x-change-window-property)
        (x-change-window-property "EMACS_SERVER_NAME" server-name (selected-frame) nil nil t nil))
      (setq frame-title-format '("Emacs:SERVER_NAME=" server-name))))

  (use-package man
    :straight nil
    :init (setq Man-notify-method 'pushy)
    :commands man)

  (use-package eww
    :straight nil
    :config
    (define-key eww-link-keymap "w" nil)
    (evil-define-key 'normal eww-mode-map
      (kbd "C-o") #'eww-back-url))

  (use-package browse-url
    :straight nil
    :init (evil-define-key '(normal motion) 'global
            (kbd "g l") #'browse-url
            (kbd "g L") #'browse-url-default-browser))

  (use-package devdocs-browser
    :straight (devdocs-browser :type git :host github :repo "blahgeek/emacs-devdocs-browser")
    :init
    (evil-define-key nil 'global
      (kbd "C-h d") #'devdocs-browser-open
      (kbd "C-h D") #'devdocs-browser-open-in)
    ;; https://github.com/emacs-evil/evil/issues/301
    (evil-define-minor-mode-key 'normal 'devdocs-browser-eww-mode
      (kbd "g s") #'devdocs-browser-eww-goto-target
      (kbd "g o") #'devdocs-browser-eww-open-in-default-browser))

  (use-package suggest)

  (comment webkit
    :init (require 'ol)
    :straight (webkit :type git :host github :repo "akirakyle/emacs-webkit"
                      :branch "main"
                      :files (:defaults "*.js" "*.css" "*.so")
                      :pre-build ("make"))
    :config
    (defun my/webkit-filter-buffer-name (args)
      "Rename webkit buffer title"
      (let ((title (car args)))
        (list (if (string= "" title)
                  title
                (concat "*Webkit* " title)))))
    (advice-add 'webkit-rename-buffer :filter-args #'my/webkit-filter-buffer-name)
    (require 'webkit-ace)
    (require 'evil-collection-webkit)
    (evil-collection-xwidget-setup)

    (modify-all-frames-parameters '((inhibit-double-buffering . t))))

  (use-package pydoc)
  )  ;; }}}

(progn  ;; Email  {{{
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        user-mail-address "yikai@z1k.dev"
        user-full-name "Yikai Zhao")

  (use-package notmuch
    :straight nil
    :init
    (evil-ex-define-cmd "nm" #'notmuch)
    :custom
    (notmuch-search-oldest-first nil)
    (notmuch-show-logo nil)
    (notmuch-archive-tags '("-inbox" "-unread"))
    :commands notmuch
    :config
    (add-to-list 'notmuch-tag-formats
                 '("gh-.*" (propertize tag 'face 'notmuch-tag-unread)))
    ;; display text/html for github notifications
    (defun my/notmuch-multipart/alternative-discouraged (msg)
      (if (string-suffix-p "@github.com" (plist-get msg :id))
          '("text/plain")
        '("text/html" "multipart/related")))
    (setq notmuch-multipart/alternative-discouraged #'my/notmuch-multipart/alternative-discouraged)

    ;; display images
    (setq notmuch-show-text/html-blocked-images nil)

    ;; shr-tag-img will ignore images with size=1
    (defun my/fix-github-email-beacon-img-dom (dom &rest _)
      (when (string-prefix-p "https://github.com/notifications/beacon/" (dom-attr dom 'src))
        (dom-set-attribute dom 'width "2")
        (dom-set-attribute dom 'height "2")))

    (advice-add 'shr-tag-img :before #'my/fix-github-email-beacon-img-dom)

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

(progn  ;; Misc {{{
  (custom-set-variables
   '(auth-source-save-behavior nil)
   '(term-buffer-maximum-size 20480)

   ;; it was reversed... (wtf?)
   ;; https://mail.gnu.org/archive/html/emacs-devel/2019-03/msg00002.html
   '(tabulated-list-gui-sort-indicator-asc ?▲)
   '(tabulated-list-gui-sort-indicator-desc ?▼))

  (use-package nsm
    :straight nil
    :config
    ;; nsm-should-check would call `network-lookup-address-info',
    ;; which calls getaddrinfo, which is a blocking call...
    ;; it happens when using notmuch on a slow connection, emacs would block for a long time.
    ;; this function is useless anyway.
    (defun my/nsm-should-check (&rest _)
      t)
    (advice-add 'nsm-should-check :override #'my/nsm-should-check))

  )  ;;; }}}

(progn  ;; Load custom.el, enable customization UI  {{{
  ;; set custom-file to another file, but only load SOME of them
  ;; steps to change variable using Customization UI: apply and save, review it, put it in this file.
  (setq custom-file "~/.emacs.d/custom.el")

  ;; Load some whitelisted variables from custom.el
  (setq my/allowed-custom-variables
        '(safe-local-variable-values))

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
