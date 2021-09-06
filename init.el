;;; init --- My config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq
 comp-deferred-compilation t
 comp-async-jobs-number 8
 comp-async-report-warnings-errors nil
 initial-major-mode 'fundamental-mode
 initial-scratch-message ";; This buffer is set to fundamental mode initially to speedup emacs startup. Execute the following line to switch back.\n;; (lisp-interaction-mode)"
 garbage-collection-messages nil)

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

  ;; DEBUG only: support "variable by package" in memory-report
  (use-package memory-report
    :demand t
    :straight nil
    :config
    (defun memory-report--largest-variables ()
      (let ((variables nil)
            (packages (make-hash-table :test 'equal))
            (total-size 0))
        (mapatoms
         (lambda (symbol)
           (when (boundp symbol)
             (let ((package (car (split-string (symbol-name symbol) "-")))
                   (size (memory-report--object-size
                          (make-hash-table :test #'eq)
                          (symbol-value symbol))))
               (cl-incf total-size size)
               (cl-incf (gethash package packages 0) size)
               (when (> size 1000)
                 (push (cons symbol size) variables)))))
         obarray)
        (list
         (cons (propertize "Memory Used By Global Variables"
                           'help-echo "Upper bound; mutually overlapping data from different variables are counted several times")
               (seq-reduce #'+ (mapcar #'cdr variables) 0))
         (with-temp-buffer
           (insert (propertize "Total: \n\n" 'face 'bold))
           (insert (memory-report--format total-size) "\n\n")

           (insert (propertize "Largest Variables\n\n" 'face 'bold))
           (cl-loop for i from 1 upto 20
                    for (symbol . size) in (seq-sort (lambda (e1 e2)
                                                       (> (cdr e1) (cdr e2)))
                                                     variables)
                    do (insert (memory-report--format size)
                               "  "
                               (symbol-name symbol)
                               "\n"))

           (insert (propertize "\nBy package\n\n" 'face 'bold))
           (let ((package-variables nil))
             (maphash (lambda (package size) (push (cons package size) package-variables))
                      packages)
             (cl-loop for i from 1 upto 20
                      for (name . size) in (seq-sort (lambda (e1 e2)
                                                       (> (cdr e1) (cdr e2)))
                                                     package-variables)
                      do (insert (memory-report--format size)
                                 "  "
                                 name
                                 "\n")))

           (buffer-string))))))

  ;;(use-package esup)
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
  )  ;; }}}

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
  )  ;; }}}

(progn  ;; EVIL & general keybindings {{{
  (when (my/macos-p)
    ;; (setq mac-command-modifier 'super
    ;;       mac-option-modifier 'meta)
    ;; Use command as control here, like (my modified) linux
    (setq mac-command-modifier 'control
          mac-control-modifier 'meta
          mac-option-modifier 'super))
  ;; EVIL depends on undo-tree anyway
  ;; diminish it
  ;; TODO: move out of this section?
  (use-package undo-tree
    :delight undo-tree-mode
    :custom
    (undo-tree-visualizer-diff t)
    (undo-tree-visualizer-timestamps t)
    :config (global-undo-tree-mode))

  (use-package evil
    :demand t
    :init
    (setq evil-want-C-w-in-emacs-state t
          evil-split-window-below t
          evil-vsplit-window-right t
          evil-want-fine-undo t
          evil-search-module 'evil-search
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
    (evil-define-key 'normal 'global
      (kbd "C-l") #'evil-ex-nohighlight)  ;; cannot bind double <escape> ?
    (evil-define-key nil 'global
      (kbd "C-S-v") #'yank)
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
    (evil-define-key 'normal 'global
      (kbd "Q") "@q"
      (kbd "C-:") #'execute-extended-command)
    ;; Use [] to replace ctrl-f and ctrl-b, saving my little finger
    (evil-define-key '(normal visual motion) 'global
      (kbd "[") #'evil-scroll-page-up
      (kbd "]") #'evil-scroll-page-down))

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
    :demand t
    :after evil
    :delight evil-commentary-mode
    :config (evil-commentary-mode t))

  (use-package evil-surround
    :demand t
    :after evil
    :config (global-evil-surround-mode t))

  (use-package vimish-fold
    :demand t
    :after evil
    :config (vimish-fold-global-mode))

  (use-package evil-vimish-fold
    :demand t
    :after vimish-fold
    :delight evil-vimish-fold-mode
    :init (evil-define-key 'normal 'global
            ;; refresh marks
            (kbd "z g") #'vimish-fold-from-marks)
    :config (global-evil-vimish-fold-mode))

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
    :config (which-key-mode t))

  )  ;; }}}

(use-package ivy  ;; {{{
  :demand t   ;; ivy-mode will make everywhere completion available
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  :delight ivy-mode
  :config
  (ivy-mode t)
  (evil-define-key '(normal motion emacs insert) 'global
    (kbd "C-r") #'ivy-switch-buffer)
  (define-key ivy-mode-map (kbd "C-j") (kbd "C-n"))
  (define-key ivy-mode-map (kbd "C-k") (kbd "C-p"))
  (define-key ivy-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)) ;; }}}

(progn  ;; Editing-related settings {{{
  (add-hook 'prog-mode-hook
            (lambda () (modify-syntax-entry ?_ "w")))

  ;; disable backup. put autosaves into .emacs.d/autosave
  (setq make-backup-files nil)
  (setq auto-save-file-name-transforms
        '((".*" "~/.emacs.d/autosave/\\1" t)))
  ;; disblae lockfiles ( .#xxx file )
  (setq create-lockfiles nil)

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
    :hook (prog-mode . outline-minor-mode)
    :delight outline-minor-mode)

  (use-package whitespace
    :straight nil
    :hook (prog-mode . whitespace-mode)
    :delight whitespace-mode
    :custom (whitespace-style '(face trailing indentation space-after-tab space-before-tab tab-mark))
    :custom-face (whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :inherit whitespace-space)))))

  (use-package hl-line
    :straight nil
    :hook (prog-mode . hl-line-mode))

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
    :hook (prog-mode . git-gutter-mode))

  (use-package rainbow-mode)

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
  :hook ((c++-mode . auto-insert-mode)
         (c-mode . auto-insert-mode)
         (python-mode . auto-insert-mode)
         (protobuf-mode . auto-insert-mode))
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
  (use-package cmake-mode)

  (use-package fish-mode)

  (use-package vimrc-mode)

  (use-package protobuf-mode
    :config
    (add-hook 'protobuf-mode-hook (lambda () (setq-local require-final-newline t))))

  (use-package gn-mode
    :mode (rx ".gn" (? "i") eos))

  (use-package bazel
    ;; https://github.com/bazelbuild/emacs-bazel-mode/issues/122
    :straight (bazel :type git :host github :repo "bazelbuild/emacs-bazel-mode")
    :custom (bazel-mode-buildifier-before-save t))

  (use-package yaml-mode
    :mode (rx ".y" (? "a") "ml" eos))

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

  (add-to-list 'auto-mode-alist `(,(rx ".mm" eos) . objc-mode))

  (setq python-prettify-symbols-alist '())

  ;; Not used by default. Add (c-file-style . "Google") to .dir-locals.el
  (use-package google-c-style
    :straight (google-c-style :fetcher github :repo "google/styleguide" :branch "gh-pages")
    :demand t
    :config (c-add-style "Google" google-c-style))

  ;; GOLANG
  (defun my/go-install-save-hooks ()
    "Install save hooks for go."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'my/go-install-save-hooks)
  ) ;;; }}}

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
    :config
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
     vterm-buffer-name-string "vterm %s"
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
    ;; Do not allow insertion commands in normal mode. Only allow "a"
    (evil-define-key 'normal vterm-mode-map
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
    (defun my/ivy-switch-buffer-vterm-only ()
      "Same as ivy-switch-buffer, but with initial input 'vterm'"
      (interactive)
      (let ((ivy-initial-inputs-alist '((ivy-switch-buffer . "^vterm "))))
        ;; variables by defcustom are always dynamic scope
        (ivy-switch-buffer)))
    (evil-define-key '(insert emacs normal motion) 'global
      (kbd "C-t") #'my/ivy-switch-buffer-vterm-only)
    ;; Must set default evil-*-state-cursor (and only once) before setting buffer-local variable
    ;; Cannot call it directly while initializing because there's no face-attribute in daemon mode
    (let ((my/vterm-setup-global-cursor-called nil))
      (defun my/vterm-setup-global-cursor (_)
        (unless my/vterm-setup-global-cursor-called
          (setq evil-normal-state-cursor `(box ,(face-attribute 'default :foreground))
                evil-insert-state-cursor `((bar . 2) ,(face-attribute 'default :foreground)))
          (setq my/vterm-setup-global-cursor-called t))))
    (defun my/vterm-init-custom ()
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
      (if (file-remote-p default-directory)
          (let ((default-directory "~/"))
            (with-editor (vterm)))  ;; cannot move with-editor out of "if"
        (with-editor (vterm))))
    (evil-ex-define-cmd "term" #'my/with-editor-vterm)
    (evil-define-key '(normal motion emacs) 'global
      (kbd "<C-return>") #'my/with-editor-vterm)

    ;; (defun my/vterm-rename-buffer-as-title (title)
    ;;   (rename-buffer (format "vterm %s" title) t))
    ;; (add-hook 'vterm-set-title-functions
    ;;           #'my/vterm-rename-buffer-as-title)

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
  )  ;; }}}

(progn  ;; Project / Window management {{{
  (use-package projectile
    :init
    ;; Set "projectile-project-name" to override the name
    (defun my/projectile-mode-line ()
      "Modified version of projectile-default-mode-line"
      (format " @%s" (or (projectile-project-name) "-")))
    (setq projectile-completion-system 'ivy
          projectile-enable-caching t
          projectile-switch-project-action #'projectile-dired
          projectile-mode-line-function #'my/projectile-mode-line
          ;; The following line is actually unused anymore
          projectile-mode-line-prefix " Proj")
    (autoload 'projectile-command-map "projectile" nil nil 'keymap)
    (evil-define-key '(normal motion emacs) 'global (kbd "C-p") 'projectile-command-map)
    :hook (prog-mode . projectile-mode)
    :config
    (projectile-mode t)
    (define-key projectile-command-map "F" 'projectile-find-file-other-window)
    (define-key projectile-command-map "h" 'projectile-find-other-file)
    (define-key projectile-command-map "H" 'projectile-find-other-file-other-window)

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
    :hook (prog-mode . yas-minor-mode)
    :delight yas-minor-mode
    :config
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
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
    :hook (prog-mode . company-mode)
    :config
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
  )  ;; }}}

(progn  ;; Flycheck  {{{
  (use-package flycheck
    :custom (flycheck-python-pylint-executable "pylint")
    :hook (prog-mode . flycheck-mode)
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
      (kbd "g !") #'flycheck-list-errors
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
     lsp-enable-file-watchers nil
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
    (defun my/lsp-deferred-with-blacklist ()
      "Same as `lsp-deferred', but blacklist certain derived modes."
      (unless (memq major-mode '(xonsh-mode))
        (lsp-deferred)))
    :hook ((c++-mode . my/lsp-deferred-with-blacklist)
           (c-mode . my/lsp-deferred-with-blacklist)
           (objc-mode . my/lsp-deferred-with-blacklist)
           (python-mode . my/lsp-deferred-with-blacklist)
           (go-mode . my/lsp-deferred-with-blacklist)
           (haskell-mode . my/lsp-deferred-with-blacklist)
           (haskell-literate-mode . my/lsp-deferred-with-blacklist)
           (js-mode . my/lsp-deferred-with-blacklist)
           (typescript-mode . my/lsp-deferred-with-blacklist)
           (web-mode . my/lsp-deferred-with-blacklist)  ;; .tsx
           (lsp-mode . lsp-enable-which-key-integration))
    :commands (lsp lsp-deferred)
    :delight
    '(" #"
      (lsp--buffer-workspaces
       (:eval (mapconcat (lambda (w) (symbol-name (lsp--workspace-server-id w)))
                         lsp--buffer-workspaces "/"))
       (:propertize "?" face warning)))
    :config
    (evil-define-key '(normal visual motion) 'global (kbd "+") #'lsp-format-region)
    (evil-define-key 'insert 'global (kbd "C-l") #'lsp-signature-activate)
    (evil-define-key nil lsp-signature-mode-map
      (kbd "C-n") #'lsp-signature-next
      (kbd "C-p") #'lsp-signature-previous
      (kbd "C-j") #'lsp-signature-next
      (kbd "C-k") #'lsp-signature-previous)
    (evil-define-key 'normal 'global
      (kbd "g r") 'lsp-find-references
      (kbd "g x") 'lsp-execute-code-action)
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
    :after lsp-mode)

  (use-package lsp-ivy
    :after (lsp-mode ivy)
    :commands lsp-ivy-workspace-symbol
    :init (evil-define-key 'normal 'global
            (kbd "g s") 'lsp-ivy-workspace-symbol))

  ;; using flycheck-posframe for flycheck error messages now
  ;; using lsp-modeline-code-actions-enable for code action now
  (comment lsp-ui
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-actions-icon nil)
    :commands lsp-ui-mode  ;; will be called by lsp
    ;; display flycheck errors using sideline even for non-lsp buffers
    ;; follow https://github.com/emacs-lsp/lsp-ui/issues/437 for future compatibility
    :hook (flycheck-mode . lsp-ui-mode)
    :config
    (evil-define-key 'normal 'global
      (kbd "g h") 'lsp-ui-doc-glance))
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
    (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-upstream))

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
    (setq dumb-jump-selector 'ivy)
    (evil-define-key 'normal 'global
      (kbd "g]") #'dumb-jump-go
      (kbd "g c-]") #'dumb-jump-go-other-window)
    :config
    (advice-add 'dumb-jump-go :before (lambda (&rest _) (evil-set-jump))))

  (use-package sudo-edit
    :init (evil-ex-define-cmd "su[do]" #'sudo-edit)
    :config
    (sudo-edit-indicator-mode))

  (use-package server
    :straight nil
    :demand t
    :unless (my/macos-p)
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
    (when (fboundp 'x-change-window-property)
      (x-change-window-property "EMACS_SERVER_NAME" server-name (selected-frame) nil nil t nil))
    (setq frame-title-format '("Emacs:SERVER_NAME=" server-name)))

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

  (use-package devdocs-browser
    :straight (devdocs-browser :type git :host github :repo "blahgeek/emacs-devdocs-browser")
    :init
    (evil-define-key nil 'global
      (kbd "C-h d") #'devdocs-browser-open
      (kbd "C-h D") #'devdocs-browser-open-in)
    :config
    ;; https://github.com/emacs-evil/evil/issues/301
    (evil-define-minor-mode-key 'normal 'devdocs-browser-eww-mode
      (kbd "g s") #'devdocs-browser-eww-goto-target
      (kbd "g o") #'devdocs-browser-eww-open-in-default-browser))

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

(progn  ;; My functions {{{
  (defun my/change-font-size ()
    "Change font size based on predefined list"
    (interactive)
    (ivy-read "Select font size:"
              (mapcar #'number-to-string
                      ;; put current selection at the end
                      (append
                       (remove my/gui-font-size-current my/gui-font-size-choices)
                       `(,my/gui-font-size-current)))
              ;; (mapcar 'number-to-string my/gui-font-size-choices)
              :action (lambda (choice) (let ((size-value (string-to-number choice)))
                                     (when (> size-value 0)
                                       (my/gui-font-size-set size-value))))))
  ;; NOTE: there's no way to implement auto-changing function
  ;; because my external monitor shares the same resolution with my laptop monitor
  (evil-define-key nil 'global
    (kbd "C-x =") #'my/change-font-size)
  )  ;; }}}

(progn  ;; Email  {{{
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        user-mail-address "yikai@z1k.dev"
        user-full-name "Yikai Zhao")
  )  ;; }}}

(progn  ;; Other Customization  {{{
  ;; mode line
  (setq-default mode-line-format
                (delete '(vc-mode vc-mode) mode-line-format))

  ;; set custom-file to another file, but only load SOME of them
  ;; steps to change variable using Customization UI: apply and save, review it, put it in this file.
  ;; TODO: move variables to seperate sections
  (setq custom-file "~/.emacs.d/custom.el")
  (custom-set-variables
   '(auth-source-save-behavior nil)
   '(blink-cursor-mode nil)
   '(c-basic-offset 4)
   '(c-default-style
     '((java-mode . "java")
       (awk-mode . "awk")
       (other . "linux")))
   '(c-tab-always-indent nil)
   '(hscroll-step 1)
   '(scroll-conservatively 101)
   '(indent-tabs-mode nil)
   '(jit-lock-defer-time 0.1)  ;; defer jit-lock, improve performance
   '(line-number-mode nil)
   '(save-place-mode t)
   '(scroll-margin 2)
   '(scroll-step 1)
   '(size-indication-mode nil)
   '(tab-always-indent nil)
   '(tab-width 4)
   '(term-buffer-maximum-size 20480)
   '(tool-bar-mode nil)
   '(truncate-lines t))
  (custom-set-faces
   '(fixed-pitch ((t (:family nil :inherit default))))
   '(line-number ((t (:height 0.9))))  ;; for pragmata, there's no light weight, let's use a smaller size
   '(mode-line ((t (:height 0.9))))  ;; smaller mode-line
   '(mode-line-inactive ((t (:background nil :inherit mode-line)))))

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
