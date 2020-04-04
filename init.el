(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)

;; (use-package gcmh
;;   :diminish gcmh-mode
;;   :init
;;   (setq gcmh-verbose t)
;;   :config
;;   (gcmh-mode t))

;; EVIL
(use-package evil
  :init
  (setq evil-want-C-w-in-emacs-state t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-fine-undo t)
  :config
  (evil-mode t)
  (evil-ex-define-cmd "bd[elete]" #'kill-current-buffer)
  (evil-ex-define-cmd "q" #'kill-buffer-and-window)
  (use-package evil-commentary
    :config (evil-commentary-mode t)
    :diminish evil-commentary-mode)
  (use-package evil-surround
    :config (global-evil-surround-mode t))
  )

(use-package vimish-fold
  :config
  (vimish-fold-global-mode t)
  (use-package evil-vimish-fold
    :config (global-evil-vimish-fold-mode t)
    :diminish evil-vimish-fold-mode))

(use-package outline
  :ensure nil
  :hook (prog-mode . outline-minor-mode)
  :diminish outline-minor-mode)

(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :diminish whitespace-mode)

(use-package hl-line
  :ensure nil
  :hook (prog-mode . hl-line-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package autoinsert
  :ensure nil
  :diminish auto-insert-mode
  :config
  (define-auto-insert
    '("\\.\\(h\\|hpp\\|hh\\)\\'" . "C++ header")
    '("Header guard"
      "#pragma once" \n \n)
    )
  (auto-insert-mode t)
  )


;; Appearance
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :config (fira-code-mode--setup)
  :diminish fira-code-mode
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

;; (use-package mode-icons
;;   :config
;;   (mode-icons-mode t))

(use-package diff-hl
  :config (global-diff-hl-mode t))

(use-package hl-todo
  :diminish hl-todo-mode
  :config (global-hl-todo-mode t))

;; (use-package diminish
;;   :config
;;   (diminish 'whitespace-mode)
;;   (diminish 'whitespace-newline-mode)
;;   (diminish 'global-whitespace-mode)
;;   (diminish 'global-whitespace-newline-mode)
;;   )

;; TODO: remove this?
(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-global-mode)
  (add-to-list 'dtrt-indent-hook-mapping-list
               '(cmake-mode default cmake-tab-width))
  )

;; TODO
;; modeline
;; indent guide
;; indent auto detect: https://github.com/jscheid/dtrt-indent
;; https://www.emacswiki.org/emacs/AutoInsertMode
;; easy align
;; magit
;; python linter

;; (use-package helm
;;   :config (require 'helm-config))

(use-package vterm
  :init
  (setq
   vterm-kill-buffer-on-exit t
   vterm-max-scrollback 10000)
  :config
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-define-key 'insert vterm-mode-map
    (kbd "C-a") 'vterm--self-insert
    (kbd "C-b") 'vterm--self-insert
    (kbd "C-c") 'vterm--self-insert
    (kbd "C-e") 'vterm--self-insert
    (kbd "C-d") 'vterm--self-insert
    (kbd "C-z") 'vterm--self-insert
    (kbd "C-w") 'vterm--self-insert
    (kbd "C-t") 'vterm--self-insert
    (kbd "C-p") 'vterm--self-insert
    (kbd "C-n") 'vterm--self-insert
    (kbd "s-v") 'vterm-yank
    )
  (defun -vterm-init-custom ()
    (make-local-variable 'evil-force-cursor)
    (make-local-variable 'evil-insert-state-cursor)
    (setq evil-force-cursor 'box
          evil-insert-state-cursor 'box)
    (evil-refresh-cursor)
    )
  (add-hook 'vterm-mode-hook '-vterm-init-custom)
  (evil-ex-define-cmd "term" #'vterm)
  )

;; (use-package term
;;   :ensure nil
;;   :config
;;   (evil-set-initial-state 'term-mode 'insert)

;;   (defun -term-switch-to-char-mode ()
;;     "Maybe switch to `term-char-mode'"
;;     (when (get-buffer-process (current-buffer))
;;       (term-char-mode)))
;;   (defun -term-sync-state-and-mode ()
;;     "Sync `term-char-mode' and `term-line-mode' with insert and normal state."
;;     (add-hook 'evil-insert-state-entry-hook '-term-switch-to-char-mode nil t)
;;     (add-hook 'evil-insert-state-exit-hook 'term-line-mode nil t))
;;   (add-hook 'term-mode-hook '-term-sync-state-and-mode)

;;   (defun -term-init-custom ()
;;     (setq scroll-margin 0))
;;   (make-local-variable 'scroll-margin)
;;   (add-hook 'term-mode-hook '-term-init-custom)

;;   (remove-hook 'comint-output-filter-functions
;;                'comint-postoutput-scroll-to-bottom)
;;   (defadvice term-handle-exit
;;       (after term-kill-buffer-on-exit activate)
;;     (kill-buffer))

;;   (evil-define-key 'insert term-raw-map
;;     (kbd "C-a") 'term-send-raw
;;     (kbd "C-c") 'term-send-raw
;;     (kbd "C-d") 'term-send-raw
;;     (kbd "C-z") 'term-send-raw
;;     (kbd "C-w") 'term-send-raw
;;     (kbd "s-v") 'term-paste
;;     )
;;   (evil-define-key 'normal term-mode-map
;;     (kbd "RET") nil
;;     (kbd "p") 'term-paste
;;     (kbd "C-k") 'term-previous-prompt
;;     (kbd "C-j") 'term-next-prompt
;;     "gk" 'term-previous-prompt
;;     "gj" 'term-next-prompt)
;;   (evil-ex-define-cmd "term"
;;                       (lambda () (interactive)
;;                         (term "/usr/bin/fish")
;;                         (rename-uniquely)))
;;   )

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
  (define-key ivy-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (evil-define-key '(normal motion emacs) 'global (kbd "C-r") 'ivy-switch-buffer))

;; EDITING
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-switch-project-action #'projectile-dired
        projectile-mode-line-prefix "Proj")
  :config
  (projectile-mode t)
  (evil-define-key '(normal motion emacs) projectile-mode-map (kbd "C-p") 'projectile-command-map)
  (define-key projectile-command-map "F" 'projectile-find-file-other-window)
  (define-key projectile-command-map "h" 'projectile-find-other-file)
  (define-key projectile-command-map "H" 'projectile-find-other-file-other-window)
  (evil-ex-define-cmd "ag" #'projectile-ag)
  )

(use-package "fzf"
  :init (setenv "FZF_DEFAULT_COMMAND" "fd --type f")
  :config
  (evil-define-key 'normal 'global (kbd "s-p") 'fzf))

(use-package eyebrowse
  :init
  (setq eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode)
  (evil-ex-define-cmd "tabc[lose]" #'eyebrowse-close-window-config)
  (evil-ex-define-cmd "tabn[ew]" #'eyebrowse-create-window-config)
  (evil-ex-define-cmd "tabs" #'eyebrowse-switch-to-window-config)
  (evil-define-key '(normal motion emacs) 'global
    (kbd "gt") #'eyebrowse-next-window-config
    (kbd "gT") #'eyebrowse-prev-window-config
    (kbd "s-t") #'eyebrowse-create-window-config
    (kbd "s-w") #'eyebrowse-close-window-config
    )
  )

(use-package company
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  :diminish company-mode
  :config
  (global-company-mode t)
  (use-package company-box  ;; this is better. does not hide line number while showing completions
    :init (setq company-box-show-single-candidate t
                company-box-doc-delay 2)
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
    )
  (evil-define-key nil company-search-map
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)
  ;; (company-tng-configure-default)
  )

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq
   lsp-clients-clangd-args '("--background-index=false" "--clang-tidy-checks=-*")
   lsp-enable-on-type-formatting nil  ;; laggy
   lsp-enable-indentation nil  ;; ???
   lsp-idle-delay 1.00
   read-process-output-max (* 1024 1024)
   )
  :config
  (lsp-mode t)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'prog-mode-hook #'lsp)
  (use-package company-lsp
    :init
    (setq company-lsp-cache-candidates 'auto)
    :config
    (push 'company-lsp company-backends))
  (use-package lsp-ui
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'at-point)
    :config
    (evil-define-key 'normal 'global (kbd "g h") 'lsp-ui-doc-glance)
    (evil-define-key 'normal 'global (kbd "g r") 'lsp-find-references)
    (evil-define-key 'normal 'global (kbd "g x") 'lsp-execute-code-action)
    )
  )

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t)
  )

(use-package magit
  :config
  (use-package evil-magit)
  (evil-define-key 'normal 'global (kbd "C-s") 'magit)
  )

(use-package ag
  :init (setq ag-highlight-search t)
  :config
  (evil-ex-define-cmd "ag" #'ag))

;; (evil-ex-define-argument-type ARG-TYPE DOC &rest BODY)

;; (use-package counsel-dash
  ;; :init
  ;; (setq dash-docs-browser-func 'xwidget-webkit-browse-url))

;; (use-package doom-modeline
;;   :config
;;   (doom-modeline-mode t))


;; Filetypes
(use-package cmake-mode)

(use-package fish-mode)

(use-package vimrc-mode)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(use-package org)


(use-package winner
  :ensure nil
  :diminish winner-mode
  :config
  (winner-mode t)
  (evil-define-key '(normal motion emacs) 'global (kbd "C-w u") 'winner-undo))

(use-package fcitx
  :init
  (setq fcitx-use-dbus t)
  :config
  (fcitx-default-setup)
  (fcitx-org-speed-command-turn-off)
  (fcitx-read-funcs-turn-off))

(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

;; (setq scroll-step 1
;;       scroll-margin 2
;;       scroll-conservatively 10000
;;       tab-width 4
;;       gc-cons-threshold 100000000
;;       read-process-output-max (* 1024 1024)
;;       )

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
  (setq gc-cons-threshold (* 1024 (* 1024 1024)))
  )

(defun my/gc-resume ()
  "Resume garbage collection (and do it once)."
  (setq gc-cons-threshold (* 100 (* 1024 1024)))
  (setq garbage-collection-messages nil)
  (message "Garbage collecting...done (%.3fs)"
           (my/timeit (garbage-collect)))
  (setq garbage-collection-messages t)
  )

(add-function :after after-focus-change-function #'my/gc-resume)
(add-hook 'evil-insert-state-exit-hook #'my/gc-resume)
(add-hook 'evil-insert-state-entry-hook #'my/gc-pause)
(add-hook 'minibuffer-setup-hook #'my/gc-pause)
(add-hook 'minibuffer-exit-hook #'my/gc-resume)


;; (electric-pair-mode t)
;; (global-hl-line-mode t)
;; (fringe-mode 16)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style
   '((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux")))
 '(c-tab-always-indent nil)
 '(electric-pair-mode t)
 '(fringe-mode 16 nil (fringe))
 '(gc-cons-threshold 100000000)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(line-number-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(fcitx vimrc-mode fish-mode vterm gcmh counsel-dash eyebrowse fzf ag hl-todo dtrt-indent flycheck mode-icons evil-magit magit evil-vimish-fold vimish-fold diminish diff-hl cmake-mode ivy lsp-ui company-box solarized-theme company-lsp company company-mode which-key use-package projectile lsp-mode evil-visual-mark-mode evil-surround evil-commentary))
 '(scroll-bar-mode nil)
 '(scroll-margin 2)
 '(scroll-step 1)
 '(show-paren-mode t)
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
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 102 :width normal))))
 '(mode-line-inactive ((t (:background nil :inherit mode-line))))
 '(whitespace-tab ((t (:foreground nil :background nil :inverse-video nil :inherit whitespace-space)))))
