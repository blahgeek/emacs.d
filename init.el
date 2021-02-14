;;; init --- My config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq
 comp-deferred-compilation t
 comp-async-jobs-number 8
 initial-major-mode 'fundamental-mode
 garbage-collection-messages t)

(progn  ;; GC tune
  ;; Set to large value before start
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
        gc-cons-percentage 0.6)
  ;; ... and restore it
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 100 (* 1024 1024))
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
  )


(progn  ;; Package Manager: straight, use-package
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
    :demand t))

(progn  ;; Some my own helper functions
  (defun my/macos-p ()
    "Return t if it's in macos."
    (string-equal system-type "darwin"))

  (defmacro my/timeit (&rest body)
    "Measure and return the time it takes to evaluate BODY."
    `(let ((time (current-time)))
       ,@body
       (float-time (time-since time)))))

(progn  ;; Appearance, setup early
  (use-package solarized-theme
    :demand t
    :config
    (load-theme 'solarized-light t))

  (use-package rainbow-mode)

  (use-package fira-code-mode
    :custom (fira-code-mode-disabled-ligatures  ;; List of ligatures to turn off
             '("[]" "#{" "#(" "#_" "#_(" "x" "{-"
               "www" "*>" "*/" "{-" "-~"))
    :config (fira-code-mode--setup)
    :delight fira-code-mode
    :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

  ;; (let ((alist `((?! . ,(regexp-opt '("!!" "!=" "!==")))
  ;;                (?# . ,(regexp-opt '("##" "###" "####" "#(" "#?" "#[" "#_" "#_(" "#{")))
  ;;                (?$ . ,(regexp-opt '("$>")))
  ;;                (?% . ,(regexp-opt '("%%")))
  ;;                (?& . ,(regexp-opt '("&&")))
  ;;                (?* . ,(regexp-opt '("*" "**" "***" "**/" "*/" "*>")))
  ;;                (?+ . ,(regexp-opt '("+" "++" "+++" "+>")))
  ;;                (?- . ,(regexp-opt '("--" "---" "-->" "-<" "-<<" "->" "->>" "-}" "-~")))
  ;;                (?. . ,(regexp-opt '(".-" ".." "..." "..<" ".=")))
  ;;                (?/ . ,(regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>")))
  ;;                (?: . ,(regexp-opt '(":" "::" ":::" ":=")))
  ;;                (?\; . ,(regexp-opt '(";;")))
  ;;                (?< . ,(regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<+" "<+>" "<-" "<--" "<->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=" "<=<" "<==" "<=>" "<>" "<|" "<|>" "<~" "<~~")))
  ;;                (?= . ,(regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>")))
  ;;                (?> . ,(regexp-opt '(">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>")))
  ;;                (?= . ,(regexp-opt '("?=")))
  ;;                (?? . ,(regexp-opt '("??")))
  ;;                (?\[ . ,(regexp-opt '("[]")))
  ;;                (?\\ . ,(regexp-opt '("\\\\" "\\\\\\")))
  ;;                (?^ . ,(regexp-opt '("^=")))
  ;;                (?w . ,(regexp-opt '("www")))
  ;;                (?x . ,(regexp-opt '("x")))
  ;;                (?{ . ,(regexp-opt '("{-")))
  ;;                (?| . ,(regexp-opt '("|=" "|>" "||" "||=")))
  ;;                (?~ . ,(regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))))))
  ;;   (dolist (char-regexp alist)
  ;;     (set-char-table-range composition-function-table (car char-regexp)
  ;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

  )
(progn  ;; EVIL & general keybindings
  (when (my/macos-p)
    ;; (setq mac-command-modifier 'super
    ;;       mac-option-modifier 'meta)
    ;; Use command as control here, like (my modified) linux
    (setq mac-command-modifier 'control
          mac-control-modifier 'meta
          mac-option-modifier 'super))
  ;; EVIL depends on undo-tree anyway
  ;; diminish it
  (use-package undo-tree
    :delight undo-tree-mode)

  (use-package evil
    :demand t
    :init
    (setq evil-want-C-w-in-emacs-state t
          evil-split-window-below t
          evil-vsplit-window-right t
          evil-want-fine-undo t
          evil-search-module 'evil-search)
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
    ;; mouse:
    ;; 1. disable drag to visual mode
    ;; 2. do not set cursor position, only focus window
    (evil-define-key 'motion 'global
      [down-mouse-1] nil)
    (evil-define-key nil 'global
      [down-mouse-1] nil
      [drag-mouse-1] nil
      ;; mouse scroll
      [C-mouse-4] nil
      [C-mouse-5] nil
      [mouse-1] #'mouse-select-window)
    (evil-define-key 'normal 'global
      "Q" "@q")
    ;; Use [] to replace ctrl-f and ctrl-b, saving my little finger
    (evil-define-key '(normal visual motion) 'global
      (kbd "[") #'evil-scroll-page-up
      (kbd "]") #'evil-scroll-page-down))

  (use-package evil-commentary
    :demand t
    :after evil
    :delight evil-commentary-mode
    :config (evil-commentary-mode t))

  (use-package evil-surround
    :demand t
    :after evil
    :config (global-evil-surround-mode t))

  (use-package disable-mouse
    :after evil)
  )

(progn  ;; Some essential utils, :demand t
  (use-package switch-buffer-functions
    :demand t)
  (use-package exec-path-from-shell
    :when (my/macos-p)
    :demand t
    :config
    (exec-path-from-shell-initialize))
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

  (use-package ivy
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
    (define-key ivy-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)))

(progn  ;; Profiling, usually disabled
;;   (use-package keyfreq
;;     :config
;;     (keyfreq-mode 1)
;;     (keyfreq-autosave-mode 1))

;;   (use-package memory-usage)

;;   (use-package esup)
  (defun display-startup-echo-area-message ()
    "Override default startup echo message."
    (message (format "Emacs started in %s, welcome" (emacs-init-time)))))


;; For mituharu emacs version only
(progn  ;; Builtin / essential tools
  (use-package outline
    :straight nil
    :hook (prog-mode . outline-minor-mode)
    :delight outline-minor-mode)

  (use-package whitespace
    :straight nil
    :hook (prog-mode . whitespace-mode)
    :delight whitespace-mode)

  (use-package hl-line
    :straight nil
    :hook (prog-mode . hl-line-mode))

  (use-package display-line-numbers
    :straight nil
    :hook (prog-mode . display-line-numbers-mode))

  (use-package autoinsert
    :straight nil
    :delight auto-insert-mode
    :hook (prog-mode . auto-insert-mode)
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
        "syntax = \"proto2\";" \n \n "package " str ";" \n \n _)))

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

  ;; delight ElDoc
  (setq eldoc-minor-mode-string nil)

  ;; avoid mouse
  (setq mouse-avoidance-banish-position '((frame-or-window . frame)
                                          (side . right)
                                          (side-pos . 0)
                                          (top-or-bottom . bottom)
                                          (top-or-bottom-pos . 0)))
  (if (display-mouse-p) (mouse-avoidance-mode 'banish))

  (use-package man
    :straight nil
    :init (setq Man-notify-method 'pushy)
    :commands man))

(progn  ;; Filetypes (Major modes)
  (use-package cmake-mode)

  (use-package fish-mode)

  (use-package vimrc-mode)

  (use-package protobuf-mode
    :config
    (add-hook 'protobuf-mode-hook (lambda () (setq-local require-final-newline t))))

  (use-package gn-mode
    :mode (rx ".gn" (? "i") eos))

  (use-package bazel-mode
    :custom (bazel-mode-buildifier-before-save t))

  (use-package yaml-mode
    :mode (rx ".y" (? "a") "ml" eos))

  (use-package kotlin-mode)

  (use-package groovy-mode)

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
  (add-hook 'go-mode-hook #'my/go-install-save-hooks))
(progn  ;; ORG mode
  (use-package org
    :init
    (setq org-directory "~/Notes"
          org-agenda-files '("~/Notes/gtd/")
          org-capture-templates '(("i" "Inbox" entry
                                   (file+olp "gtd/inbox.org" "Inbox")
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
    (evil-define-key 'normal org-tree-slide-mode-map
      (kbd "{") #'org-tree-slide-move-previous-tree
      (kbd "}") #'org-tree-slide-move-next-tree)))

(progn  ;; Basic editing experience
  (use-package origami
    :hook (prog-mode . origami-mode))
  ;; git-gutter is better than diff-hl
  (use-package git-gutter-fringe
    :delight git-gutter-mode
    :init
    ;; by default, git-gutter-mode will autoload "git-gutter" without fringe
    (autoload 'git-gutter-mode "git-gutter-fringe" nil t)
    :hook (prog-mode . git-gutter-mode))

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

  (add-hook 'prog-mode-hook
            (lambda () (modify-syntax-entry ?_ "w"))))

(progn  ;; VTerm
  (use-package with-editor
    :commands with-editor)
  (use-package vterm
    :straight (vterm :fetcher github :repo "blahgeek/emacs-libvterm" :branch "blah")
    :demand t
    :init
    (setq
     vterm-kill-buffer-on-exit t
     vterm-max-scrollback 10000
     vterm-buffer-name-string "vterm %s")
    :config
    (setf (alist-get "man" vterm-eval-cmds nil nil #'string=) '(man))
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
      (kbd "C-r") nil  ;; allow use C-r to find buffer in insert mode
      (kbd "C-S-v") 'vterm-yank)
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
      (evil-refresh-cursor))
    (add-hook 'vterm-mode-hook #'my/vterm-init-custom)
    ;; needed by emacs 28. force refresh cursor after switching buffer
    (add-hook 'switch-buffer-functions
              (lambda (&rest _) (evil-refresh-cursor)))
    (defun my/with-editor-vterm ()
      (interactive)
      (with-editor (vterm)))
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
              buf)))))
(progn  ;; Project / Window management
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
    (evil-ex-define-cmd "ag" #'projectile-ag)
    (evil-define-key '(normal motion emacs) 'global (kbd "C-p") 'projectile-command-map)
    :config
    (projectile-mode t)
    (define-key projectile-command-map "F" 'projectile-find-file-other-window)
    (define-key projectile-command-map "h" 'projectile-find-other-file)
    (define-key projectile-command-map "H" 'projectile-find-other-file-other-window))

  (use-package winner
    :demand t
    :straight nil
    :delight winner-mode
    :config
    (winner-mode t)
    (evil-define-key '(normal motion emacs) 'global
      (kbd "C-w u") 'winner-undo
      (kbd "C-w x") 'kill-this-buffer)))

(progn  ;; LSP, Completion
  (use-package company
    :init
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0  ;; default is 0.2
          ;; NOTE: revert this if it's slow
          company-search-regexp-function 'company-search-flex-regexp
          company-tooltip-align-annotations t
          ;; show single candidate as tooltip
          company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
    :delight company-mode
    :hook (prog-mode . company-mode)
    :config
    ;; Bring company-capf to front for lsp
    (setq company-backends (delete #'company-capf company-backends))
    (add-to-list 'company-backends #'company-capf)
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
      ;; the completion popup will not disappear while working with lsp and capf
      ;; https://github.com/emacs-lsp/lsp-mode/issues/1447
      (kbd "<escape>") (lambda () (interactive) (company-abort) (evil-normal-state)))
    (evil-define-key nil company-search-map
      (kbd "C-j") 'company-select-next-or-abort
      (kbd "C-k") 'company-select-previous-or-abort
      (kbd "<escape>") 'company-search-abort)
    ;; (company-tng-configure-default)
    )

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
    ;; optimize flycheck-list-errors buffer
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.20)))
    ;; from evil-collection
    (evil-define-key 'normal 'flycheck-error-list-mode-map
      (kbd "C-j") 'flycheck-error-list-next-error
      (kbd "C-k") 'flycheck-error-list-previous-error
      "gr" 'flycheck-error-list-check-source
      "?" 'flycheck-error-list-explain-error
      (kbd "RET") 'flycheck-error-list-goto-error
      "q" 'quit-window)

    ;; Language specific settings
    ;; https://github.com/flycheck/flycheck/issues/1475
    ;; otherwise, it runs eslint --print-config, which is slow
    (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (use-package flycheck-google-cpplint
    :after flycheck
    :custom (flycheck-c/c++-googlelint-executable "cpplint")
    :demand t)

  (use-package flycheck-pos-tip
    :hook (flycheck-mode . flycheck-pos-tip-mode))

  (use-package lsp-mode
    :init
    (setq
     lsp-keymap-prefix "C-S-l"
     lsp-clients-clangd-args '("--background-index=false" "--header-insertion-decorators" "--log=verbose")
     lsp-enable-snippet nil
     lsp-enable-on-type-formatting nil  ;; laggy
     lsp-enable-indentation nil  ;; disable lsp-format using evil "=". use "+" for lsp-format. see below
     lsp-keep-workspace-alive nil   ;; close lang servers on closing project
     lsp-enable-file-watchers nil
     lsp-idle-delay 1.00
     read-process-output-max (* 1024 1024)
     lsp-signature-auto-activate nil  ;; disable auto activate. use "C-l" to trigger
     lsp-prefer-capf t
     lsp-modeline-code-actions-enable nil
     ;; disable breadcrumb by default, enable by "prefix T b"
     lsp-headerline-breadcrumb-enable nil
     ;; we already have flycheck, no need for extra modeline diagnostics
     lsp-modeline-diagnostics-enable nil)
    :hook ((c++-mode . lsp-deferred)
           (c-mode . lsp-deferred)
           (objc-mode . lsp-deferred)
           (python-mode . lsp-deferred)
           (go-mode . lsp-deferred)
           (haskell-mode . lsp-deferred)
           (haskell-literate-mode . lsp-deferred)
           (js-mode . lsp-deferred)
           (typescript-mode . lsp-deferred)
           (web-mode . lsp-deferred)  ;; .tsx
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
      (kbd "C-k") #'lsp-signature-previous))

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

  (use-package lsp-ui
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-include-signature t)
    :commands lsp-ui-mode  ;; will be called by lsp
    :config
    (evil-define-key 'normal 'global
      (kbd "g h") 'lsp-ui-doc-glance
      (kbd "g r") 'lsp-find-references
      (kbd "g x") 'lsp-execute-code-action))
  )
(progn  ;; External integration
  ;; (use-package magit
  ;;   :init
  ;;   (evil-define-key 'normal 'global (kbd "C-s") 'magit)
  ;;   ;; Too slow in some projects
  ;;   (setq magit-commit-show-diff nil)
  ;;   :commands magit
  ;;   :config
  ;;   (remove-hook 'magit-status-headers-hook #'magit-insert-upstream-branch-header)
  ;;   (remove-hook 'magit-status-headers-hook #'magit-insert-push-branch-header)
  ;;   (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header))

  ;; (use-package evil-magit
  ;;   :after magit
  ;;   :demand t)

  (use-package ag
    :init
    (setq ag-highlight-search t)
    (evil-ex-define-cmd "ag" #'ag))

  (use-package fcitx
    :demand t
    :init
    (unless (my/macos-p)
      (setq fcitx-use-dbus t))
    :config
    (fcitx-evil-turn-on))

  ;; (use-package counsel-dash
  ;; :init
  ;; (setq dash-docs-browser-func 'xwidget-webkit-browse-url))

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
    (x-change-window-property "EMACS_SERVER_NAME" server-name (selected-frame) nil nil t nil))

  (use-package add-node-modules-path
    :hook js-mode)

  (use-package pydoc))

(use-package tramp)

(progn  ;; My functions
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
    (kbd "C-x =") #'my/change-font-size))

(progn  ;; Customize
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
   '(indent-tabs-mode nil)
   '(line-number-mode nil)
   '(save-place-mode t)
   '(scroll-margin 2)
   '(scroll-step 1)
   '(size-indication-mode nil)
   '(tab-always-indent nil)
   '(tab-width 4)
   '(term-buffer-maximum-size 20480)
   '(tool-bar-mode nil)
   '(truncate-lines t)
   '(whitespace-style
     '(face trailing empty indentation space-after-tab space-before-tab tab-mark)))
  (custom-set-faces
   '(mode-line-inactive ((t (:background nil :inherit mode-line))))
   '(whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :inherit whitespace-space)))))

  ;; Load some whitelisted variables from custom.el
  (setq my/allowed-custom-variables
        '(safe-local-variable-values))

  (when-let ((content (with-temp-buffer
                        (insert-file-contents-literally custom-file)
                        (goto-char (point-min))
                        (read (current-buffer))))
             ;; content => (custom-set-variables (quote (k1 v2)) (quote (k2 v2)))
             (is-custom-variable (eq (car content) 'custom-set-variables))
             (filtered-variables (seq-filter
                                  (lambda (e) (memq (car (cadr e)) my/allowed-custom-variables))
                                  (cdr content))))
    (apply 'custom-set-variables (mapcar 'cadr filtered-variables))))
