;;; init --- My config -*- lexical-binding: t -*-

;;; Commentary:

(setq comp-deferred-compilation t)

;;; straight.el
(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 20)

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

(straight-use-package 'use-package)

(use-package diminish)

;; Some helper function
(defun my/macos-p ()
    "Return t if it's in macos."
    (memq window-system '(mac ns)))

(unless (my/macos-p)
  (use-package fringe-scale
    :straight (emacs-fringe-scale :type git :host github :repo "blahgeek/emacs-fringe-scale")
    :config (fringe-scale-setup)))

(use-package switch-buffer-functions)

;; For mituharu emacs version only
(when (my/macos-p)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

(when (my/macos-p)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package all-the-icons
  :init (setq inhibit-compacting-font-caches t
              all-the-icons-scale-factor 0.8
              all-the-icons-default-adjust 0.0))
;; (all-the-icons-install-fonts)

;; TODO
;; modeline
;; indent guide
;; easy align
;; python linter

;; EVIL

;; EVIL depends on undo-tree anyway
;; diminish it
(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package evil
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
  (use-package evil-commentary
    :config (evil-commentary-mode t)
    :diminish evil-commentary-mode)
  (use-package evil-surround
    :config (global-evil-surround-mode t)))

(use-package vimish-fold
  :config
  (vimish-fold-global-mode t)
  (use-package evil-vimish-fold
    :config (global-evil-vimish-fold-mode t)
    :diminish evil-vimish-fold-mode))


;; Builtin package config

(use-package outline
  :straight nil
  :hook (prog-mode . outline-minor-mode)
  :diminish outline-minor-mode)

(use-package whitespace
  :straight nil
  :hook (prog-mode . whitespace-mode)
  :diminish whitespace-mode)

(use-package hl-line
  :straight nil
  :hook (prog-mode . hl-line-mode))

(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package autoinsert
  :straight nil
  :diminish auto-insert-mode
  :config
  (define-auto-insert
    '("\\.\\(h\\|hpp\\|hh\\)\\'" . "C++ header")
    '(nil
      "#pragma once" \n \n))
  (define-auto-insert
    '("\\.\\(py\\)\\'" . "Python header")
    '(nil
      "#!/usr/bin/env python3" \n "# -*- coding: utf-8 -*-" \n \n))
  (auto-insert-mode t))

(use-package elec-pair
  :straight nil
  :init (setq electric-pair-skip-whitespace nil)
  :config (electric-pair-mode t))

(use-package paren
  :straight nil
  :init (setq show-paren-when-point-inside-paren t)
  :config (show-paren-mode t))

(use-package autorevert
  :straight nil
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package man
  :straight nil
  :init (setq Man-notify-method 'pushy))

;; Filetypes
(use-package cmake-mode)

(use-package fish-mode)

(use-package vimrc-mode)

(use-package protobuf-mode)

(use-package gn-mode
  :mode "\\.gni?\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package kotlin-mode)

(use-package groovy-mode)

(use-package markdown-mode
  :init (setq markdown-command "markdown2"))

(use-package go-mode)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(use-package org
  :init (setq org-directory "~/Notes"
              org-agenda-files '("~/Notes/kwai/")
              org-capture-templates '(("k" "Kwai" entry (file+headline "kwai/kwai.org" "Incoming")
                                       "* TODO %?\n  %i\n  %a")))
  :config (evil-define-key '(normal motion emacs) 'global
            (kbd "s-o l") #'org-store-link
            (kbd "s-o a") #'org-agenda
            (kbd "s-o c") #'org-capture))

;; Appearance
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :config (fira-code-mode--setup)
  :diminish fira-code-mode
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

;; git-gutter is better than diff-hl
(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config (global-git-gutter-mode t))

(use-package hl-todo
  :diminish hl-todo-mode
  :config (global-hl-todo-mode t))

;; Terminal

(use-package vterm
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
    (kbd "s-v") 'vterm-yank)
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
  (evil-ex-define-cmd "term" #'vterm)
  (evil-define-key '(normal motion emacs) 'global
    (kbd "<s-return>") #'vterm
    (kbd "<s-S-return>") #'vterm-other-window
    (kbd "<M-return>") #'vterm
    (kbd "<M-S-return>") #'vterm-other-window)
  ;; (defun my/vterm-rename-buffer-as-title (title)
  ;;   (rename-buffer (format "vterm %s" title) t))
  ;; (add-hook 'vterm-set-title-functions
  ;;           #'my/vterm-rename-buffer-as-title)
  )

;; TOOLS

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (define-key ivy-mode-map (kbd "C-j") (kbd "C-n"))
  (define-key ivy-mode-map (kbd "C-k") (kbd "C-p"))
  (define-key ivy-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit))

;; Project/window Management

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-switch-project-action #'projectile-dired
        projectile-mode-line-prefix " Proj")
  :config
  (projectile-mode t)
  (evil-define-key '(normal motion emacs) projectile-mode-map (kbd "C-p") 'projectile-command-map)
  (define-key projectile-command-map "F" 'projectile-find-file-other-window)
  (define-key projectile-command-map "h" 'projectile-find-other-file)
  (define-key projectile-command-map "H" 'projectile-find-other-file-other-window)
  (evil-ex-define-cmd "ag" #'projectile-ag)
  )

(use-package perspective
  :init (setq persp-mode-prefix-key (kbd "C-S-w")
              persp-show-modestring nil)
  :config
  (persp-mode)
  (evil-define-key '(normal motion emacs) 'global
    (kbd "C-S-r") #'ivy-switch-buffer
    (kbd "C-r") #'persp-ivy-switch-buffer))

(use-package winner
  :straight nil
  :diminish winner-mode
  :config
  (winner-mode t)
  (evil-define-key '(normal motion emacs) 'global
    (kbd "C-w u") 'winner-undo
    (kbd "C-w x") 'kill-this-buffer))

;; EDITING

(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-global-mode)
  (add-to-list 'dtrt-indent-hook-mapping-list
               '(cmake-mode default cmake-tab-width)))

(use-package company
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  :diminish company-mode
  :config
  (global-company-mode t)
  ;; Bring company-capf to front for lsp
  (setq company-backends (delete #'company-capf company-backends))
  (add-to-list 'company-backends #'company-capf)
  (use-package company-box  ;; this is better. does not hide line number while showing completions
    :init (setq company-box-show-single-candidate t
                company-box-doc-delay 1
                company-box-icons-alist 'company-box-icons-all-the-icons)
    :hook (company-mode . company-box-mode)
    :diminish company-box-mode)
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
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)
  ;; (company-tng-configure-default)
  )

(use-package flycheck
  :config (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq
   lsp-clients-clangd-args '("--background-index=false" "--header-insertion-decorators")
   lsp-enable-on-type-formatting nil  ;; laggy
   lsp-enable-indentation nil  ;; disable lsp-format using evil "=". use "+" for lsp-format. see below
   lsp-keep-workspace-alive nil   ;; close lang servers on closing project
   lsp-enable-file-watchers nil
   lsp-idle-delay 1.00
   read-process-output-max (* 1024 1024)
   lsp-signature-auto-activate nil  ;; disable auto activate. use "C-l" to trigger
   lsp-prefer-capf t
   lsp-modeline-code-actions-enable nil
   ;; we already have flycheck, no need for extra modeline diagnostics
   lsp-modeline-diagnostics-enable nil)
  :config
  (use-package lsp-ui
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-include-signature t)
    :config
    (evil-define-key 'normal 'global (kbd "g h") 'lsp-ui-doc-glance)
    (evil-define-key 'normal 'global (kbd "g r") 'lsp-find-references)
    (evil-define-key 'normal 'global (kbd "g x") 'lsp-execute-code-action)
    ;; TODO: "g s" documentSymbol
    )

  (evil-define-key '(normal visual motion) 'global (kbd "+") #'lsp-format-region)
  (evil-define-key 'insert 'global (kbd "C-l") #'lsp-signature-activate)
  (evil-define-key nil lsp-signature-mode-map
    (kbd "C-n") #'lsp-signature-next
    (kbd "C-p") #'lsp-signature-previous
    (kbd "C-j") #'lsp-signature-next
    (kbd "C-k") #'lsp-signature-previous)

  (use-package lsp-java
    :init (setq lsp-java-configuration-maven-user-settings (expand-file-name "~/.m2/settings.xml")))
  (use-package lsp-pyright)

  (lsp-mode t)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  ;; autostart for some languages
  (dolist (m '(c++-mode-hook
               c-mode-hook
               objc-mode-hook
               python-mode-hook
               go-mode-hook))
    (add-hook m #'lsp-deferred)))

;; GOLANG
(defun my/go-install-save-hooks ()
  "Install save hooks for go."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'my/go-install-save-hooks)

;; Other tools

(use-package magit
  :config
  (use-package evil-magit)
  (evil-define-key 'normal 'global (kbd "C-s") 'magit))

(use-package ag
  :init (setq ag-highlight-search t)
  :config
  (evil-ex-define-cmd "ag" #'ag))

(use-package fcitx
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
  :config
  (evil-define-key 'normal 'global
    (kbd "g]") #'dumb-jump-go
    (kbd "g c-]") #'dumb-jump-go-other-window)
  (advice-add 'dumb-jump-go :before (lambda (&rest _) (evil-set-jump))))

(use-package pydoc)

;; Other custom configs

(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(evil-define-key 'normal 'global
  "Q" "@q")

(setq garbage-collection-messages t)

(defmacro my/timeit (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun my/gc-pause ()
  "Pause garbage collection for now."
  (setq gc-cons-threshold (* 2 (* 1024 (* 1024 1024))))
  )

(defun my/gc-resume ()
  "Resume garbage collection (and do it once)."
  (setq gc-cons-threshold (* 64 (* 1024 1024)))
  (setq garbage-collection-messages nil)
  (message "Garbage collecting...done (%.3fs)"
           (my/timeit (garbage-collect)))
  (setq garbage-collection-messages t)
  )

(add-hook 'focus-out-hook #'my/gc-resume)
(add-hook 'evil-insert-state-exit-hook #'my/gc-resume)
(add-hook 'evil-insert-state-entry-hook #'my/gc-pause)
(add-hook 'minibuffer-setup-hook #'my/gc-pause)
(add-hook 'minibuffer-exit-hook #'my/gc-resume)

(set-fringe-mode (if (my/macos-p) 8 16))
(menu-bar-mode (if (my/macos-p) t 0))

;; Just like custom-set-faces
(custom-theme-set-faces
 'user
 `(default ((t (:family "Fira Code"
                :foundry "CTDB"
                :slant normal
                :weight normal
                :height ,(if (my/macos-p) 140 102)
                :width normal))))
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style
   '((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux")))
 '(c-tab-always-indent nil)
 '(gc-cons-threshold 100000000)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(line-number-mode nil)
 '(make-backup-files nil)
 '(mode-line-percent-position nil)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-inactive ((t (:background nil :inherit mode-line))))
 '(whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :inherit whitespace-space)))))
