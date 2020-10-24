;;; early-init.el ---                                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my/macos-p ()
  "Return t if it's in macos."
  (string-equal system-type "darwin"))

;;; GUI dependent settings, read before setting fonts
;;; Override them in early-init-custom.el in different machine
(defvar my/gui-font-size-choices (if (my/macos-p) '(140) '(102))
  "List of integers as choices of font size (height).")
(defvar my/gui-fringe-size (if (my/macos-p) 8 16))

(let ((my/-early-init-custom-file (expand-file-name "early-init-custom.el" user-emacs-directory)))
  (when (file-exists-p my/-early-init-custom-file)
    (load-file my/-early-init-custom-file)))

(set-fringe-mode my/gui-fringe-size)
(menu-bar-mode (if (my/macos-p) t 0))
(scroll-bar-mode -1)

(defvar my/gui-font-size-current (car my/gui-font-size-choices))

(defun my/gui-font-size-set (value)
  "Set gui font with size VALUE."
  (setq my/gui-font-size-current value)
  (set-face-attribute 'default nil
                    :family "Fira Code"
                    :foundry "CTDB"
                    :slant 'normal
                    :weight 'normal
                    :height value
                    :width 'normal))

(my/gui-font-size-set (car my/gui-font-size-choices))
