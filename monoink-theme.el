(deftheme monoink
  "Created 2023-02-17."
  'theme-immediate)


;; VALUE specifies the weight of the font to use.  It must be one of
;; the symbols ‘ultra-heavy’, ‘heavy’ (a.k.a. ‘black’),
;; ‘ultra-bold’ (a.k.a. ‘extra-bold’), ‘bold’,
;; ‘semi-bold’ (a.k.a. ‘demi-bold’), ‘medium’, ‘normal’ (a.k.a. ‘regular’,
;; a.k.a. ‘book’), ‘semi-light’ (a.k.a. ‘demi-light’),
;; ‘light’, ‘extra-light’ (a.k.a. ‘ultra-light’), or ‘thin’.

;; VALUE specifies the slant of the font to use.  It must be one of the
;; symbols ‘italic’, ‘oblique’, ‘normal’, ‘reverse-italic’, or
;; ‘reverse-oblique’.

(custom-theme-set-faces
 'monoink
 '(default ((t (:family "Iosevka Ink" :foundry "FSD " :width normal :height 110 :weight normal :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil
                :foreground "black" :background "white" :stipple nil :inherit nil))))
 '(cursor ((t (:inverse-video t :background "black" :foreground "white"))))
 '(fixed-pitch ((t (:inherit (default)))))
 '(variable-pitch ((t (:family "Iosevka Ink Aile"))))
 '(escape-glyph ((t (:foreground "black"))))
 '(homoglyph ((t (:foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "black"))))
 '(highlight ((t (:background "gray90"))))
 '(region ((t (:background "gray80" :extend t))))
 '(shadow ((t (:foreground "black" :weight light))))
 '(secondary-selection ((t (:background "gray70" :extend t))))
 '(trailing-whitespace ((t (:underline (:style wave)))))
 '(whitespace-trailing ((t (:underline (:style wave)))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:slant oblique :weight bold :foreground "black"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "black" :slant italic :weight light))))
 '(font-lock-comment-face ((t (:foreground "black" :weight light :slant normal))))
 '(font-lock-constant-face ((t (:weight bold :foreground "black"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:slant italic :foreground "black"))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-name-face ((t (:foreground "black"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "black"))))
 '(font-lock-negation-char-face ((t (:weight bold :foreground "black"))))
 '(font-lock-number-face ((t nil)))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "black"))))
 '(font-lock-property-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground "black"))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold :foreground "black"))))
 '(font-lock-string-face ((t (:foreground "black" :slant oblique))))
 '(font-lock-type-face ((t (:foreground "black" :underline t))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(font-lock-warning-face ((t (:weight bold :inherit (error)))))
 '(button ((t (:underline (:color foreground-color :style line :position nil)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line :position nil) :foreground "black"))))
 '(link-visited ((t (:weight normal :underline (:color foreground-color :style line :position nil) :foreground "black"))))
 '(fringe ((t (:background "white" :foreground "black"))))
 '(header-line ((t (:box (:line-width (2 . 2) :color "gray80" :style unspecified) :background "gray80" :foreground "black" :underline (:color "gray80" :style line :position nil) :overline nil :inverse-video unspecified))))
 '(tooltip ((t (:inherit (default) :foreground "black" :background "gray90" :box 2))))
 '(mode-line ((t (:height 0.9 :underline (:color "gray80" :style line :position nil) :overline "gray80" :box (:line-width (1 . 1) :color "gray80" :style unspecified) :foreground "black" :background "gray80"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "black"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:underline (:color "gray80" :style line :position nil) :overline "gray80" :box (:line-width (1 . 1) :color "gray80" :style unspecified) :foreground "black" :inherit (mode-line)))))

 '(line-number ((t (:weight thin :height 0.9))))
 '(show-paren-match ((t (:background "gray70"))))

 ;; term
 '(term-color-black ((t (:foreground "black" :background "black"))))
 '(term-color-red ((t (:foreground "black" :background "gray70"))))
 '(term-color-green ((t (:foreground "black" :background "gray70"))))
 '(term-color-yellow ((t (:foreground "black" :background "gray70"))))
 '(term-color-blue ((t (:foreground "black" :background "gray70"))))
 '(term-color-magenta ((t (:foreground "black" :background "gray70"))))
 '(term-color-cyan ((t (:foreground "black" :background "gray70"))))
 '(term-color-white ((t (:foreground "white" :background "white"))))
 '(term-color-bright-black ((t (:foreground "gray10" :background "gray20"))))
 '(term-color-bright-red ((t (:foreground "black" :background "gray90"))))
 '(term-color-bright-green ((t (:foreground "black" :background "gray90"))))
 '(term-color-bright-yellow ((t (:foreground "black" :background "gray90"))))
 '(term-color-bright-blue ((t (:foreground "black" :background "gray90"))))
 '(term-color-bright-magenta ((t (:foreground "black" :background "gray90"))))
 '(term-color-bright-cyan ((t (:foreground "black" :background "gray90"))))
 '(term-color-bright-white ((t (:foreground "white" :background "white"))))

 '(warning ((t (:weight bold :underline (:style wave) :foreground "black"))))
 '(error ((t (:weight heavy :underline (:style wave) :foreground "black"))))
 '(flycheck-warning ((t (:underline (:style wave :color "black")))))
 '(flycheck-error ((t (:underline (:style wave :color "black")))))
 '(flycheck-posframe-background-face ((t (:background "white"))))

 '(orderless-match-face-0 ((t (:foreground "black" :weight bold :underline t))))
 '(orderless-match-face-1 ((t (:foreground "black" :weight bold :underline t))))
 '(orderless-match-face-2 ((t (:foreground "black" :weight bold :underline t))))
 '(orderless-match-face-3 ((t (:foreground "black" :weight bold :underline t))))
 '(evil-ex-substitute-replacement ((t (:foreground "black" :underline t :weight bold))))
 '(completions-common-part ((t (:foreground "black" :weight bold))))

 '(magit-section-heading ((t (:foreground "black" :background "white" :weight heavy))))
 '(magit-diff-hunk-heading ((t (:foreground "black" :background "white" :weight bold))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "black" :background "gray70" :weight bold))))
 '(magit-diff-hunk-heading-selection ((t (:foreground "black" :background "white" :weight bold))))
 '(magit-diff-lines-heading ((t (:foreground "black" :background "white" :weight bold))))
 '(magit-diff-added ((t :background "white" :foreground "black")))
 '(magit-diff-added-highlight ((t :background "white" :foreground "black")))
 '(magit-diff-removed ((t :background "white" :foreground "black" :slant oblique)))
 '(magit-diff-removed-highlight ((t :background "white" :foreground "black" :slant oblique)))
 '(magit-diff-base ((t :background "white" :foreground "black" :weight light)))
 '(magit-diff-base-highlight ((t :background "white" :foreground "black" :weight light)))
 '(magit-diff-context ((t :background "white" :foreground "black" :weight light)))
 '(magit-diff-context-highlight ((t :background "white" :foreground "black" :weight light)))
 '(magit-diffstat-added ((t :background "white" :foreground "black")))
 '(magit-diffstat-removed ((t :background "white" :foreground "black")))

 '(company-tooltip ((t (:background "white" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "gray90" :foreground "black"))))
 '(company-tooltip-common ((t (:foreground "black" :weight bold))))
 '(company-tooltip-annotation ((t (:foreground "black" :weight light))))

 '(outline-1 ((t (:weight heavy))))

 '(hl-todo ((t :foreground "white" :weight heavy)))

 '(git-gutter:modified ((t :foreground "black")))
 '(git-gutter:added ((t :foreground "black")))
 '(git-gutter:deleted ((t :foreground "black")))

 '(isearch ((t (:weight normal :background "gray70" :foreground "black" :underline (:width 2)))))
 '(isearch-fail ((t (:bold t :background "gray70" :foreground "black" :underline t))))
 '(lazy-highlight ((t (:weight normal :background "gray70" :foreground "black"))))
 '(match ((t (:weight bold :foreground "black" :background "gray70"))))
 '(next-error ((t (:weight normal :background "#fdbac6" :foreground "#854568"))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'monoink)
