;;; early-init.el ---                                -*- lexical-binding: t; -*-

;;; Commentary:
;;; Set GUI related settings here so that the startup process would be faster

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

(set-face-attribute 'default nil
                    :family "PragmataPro Liga"
                    :slant 'normal
                    :weight 'normal
                    :height (car my/gui-font-size-choices)
                    :width 'expanded)
;; (set-face-attribute 'variable-pitch nil
;;                     :family "Iosevka Blah Proportional"
;;                     :width 'normal)

(defvar my/gui-font-size-current (car my/gui-font-size-choices))

(defun my/gui-font-size-set (value)
  "Set gui font with size VALUE."
  (setq my/gui-font-size-current value)
  (set-face-attribute 'default nil :height value))

;;; early-init.el ends here
