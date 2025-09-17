;;; early-init.el ---                                -*- lexical-binding: t; -*-

;;; Commentary:
;;; Set GUI related settings here so that the startup process would be faster

;;; Code:

(setq package-enable-at-startup nil)

(defun my/macos-p ()
  "Return t if it's in macos."
  (string-equal system-type "darwin"))

;;; GUI dependent settings, read before setting fonts
;;; Override them in early-init-custom.el in different machine
(defvar my/gui-font-size-choices (if (my/macos-p) '(180) '(102))
  "List of integers as choices of font size (height).")
(defvar my/gui-fringe-size (if (my/macos-p) 8 16))

(let ((my/-early-init-local-file (expand-file-name "early-init-local.el" user-emacs-directory)))
  (when (file-exists-p my/-early-init-local-file)
    (load-file my/-early-init-local-file)))

;; checking `window-system' or `display-graphic-p' does not work in early-init.el
(when initial-window-system
  (set-fringe-mode my/gui-fringe-size)
  (scroll-bar-mode -1)
  ;; required to remove margin on macOS fullscreen
  (setq frame-resize-pixelwise t))

(setenv "LSP_USE_PLISTS" "true")

(when (my/macos-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-proxy-icon nil))

(unless (and (my/macos-p) initial-window-system)
  (menu-bar-mode 0))

(unless (my/macos-p)
  (setq frame-inhibit-implied-resize t)  ;; for tile-WM; speedup
  )

;; using Mono font is better, because emacs `char-width' would incorrectly return "1" for many chars (e.g. — (U+2014))
(set-face-attribute 'default nil
                    :family "PragmataPro Mono Liga"
                    :slant 'normal
                    :weight 'normal
                    :height (car my/gui-font-size-choices)
                    :width 'expanded)

(defvar my/gui-font-size-current (car my/gui-font-size-choices))

(defun my/gui-font-size-set (value)
  "Set gui font with size VALUE."
  (setq my/gui-font-size-current value)
  (set-face-attribute 'default nil :height value))

;;; early-init.el ends here
