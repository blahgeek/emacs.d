;;; early-init.el ---                                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my/macos-p ()
  "Return t if it's in macos."
  (memq window-system '(mac ns)))

(set-fringe-mode (if (my/macos-p) 8 16))
(menu-bar-mode (if (my/macos-p) t 0))
(scroll-bar-mode -1)

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :foundry "CTDB"
                    :slant 'normal
                    :weight 'normal
                    :height (if (my/macos-p) 140 102)
                    :width 'normal)
