;;; ag-themes-spacemacs-light-theme.el --- Customized theme based on spacemacs-light -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: faces
;; License: GPL v3
;; Package-Requires: ((emacs "27"))
;; Version: 1.0.0
;;
;;; Commentary:
;;
;;  Description
;; Customized theme based on spacemacs-light
;;; Code:

(require 'ag-themes)
(require 'spacemacs-light-theme)

(deftheme ag-themes-spacemacs-light "Customized theme based on spacemacs-light.")

(let* ((default-background "#fbf8ef")
       (default-foreground "#655370")
       (bg-darker "#f7f4eb")
       (bg-accent "#dedae0")
       (bg-accent-dark "#c7c1c9")
       (bg-accent-darker "#b1adb3")
       (bg-accent-light "#efedf0")
       (fg-accent "CadetBlue")
       (faces `((default (:background (lighter 0)))
                (window-divider (:foreground (lighter default :background 0)))
                (window-divider-first-pixel (:foreground (lighter default :background 10)))
                (window-divider-last-pixel (:foreground (lighter default :background 10)))
                (mode-line (:underline unspecified :box (:color ,bg-accent) :background (darker default :background 8)))
                (mode-line-inactive (:underline unspecified :box (:color ,bg-accent) :background (darker default :background 2)))
                (doom-modeline (:inherit mode-line))
                (doom-modeline-bar (:background (darker default :background 5)))
                (doom-modeline-bar-inactive (:inherit fringe))
                (cursor (:background "DarkGoldenrod2"))

                (dired-subtree-depth-1-face (:background (darker dired-directory :background 2)))
                (dired-subtree-depth-2-face (:background (darker dired-directory :background 6)))
                (dired-subtree-depth-3-face (:background (darker dired-directory :background 8)))
                (dired-subtree-depth-4-face (:background (darker dired-directory :background 10)))
                (dired-subtree-depth-5-face (:background (darker dired-directory :background 15)))
                (dired-subtree-depth-6-face (:background (darker dired-directory :background 20)))

                ;; (region (:inverse-video t :foreground (darker default :background 20) :background unspecified :distant-foreground unspecified))
                (fringe (:background (darker default :background 2)))
                (shadow (:foreground ,bg-accent-dark))
                (magit-diff-hunk-heading (:background ,bg-darker))
                (magit-diff-file-heading-highlight (:background ,bg-accent-light))
                (magit-diff-hunk-heading-highlight (:background ,bg-accent-light))
                (magit-diff-context-highlight (:background ,bg-darker))
                (magit-diff-added (:foreground "#67963d" :background "#e6ffed"))
                (magit-diff-added-highlight (:foreground "#325e0b" :background "#e6ffed"))
                (magit-diff-removed (:foreground "#ef6160" :background "#ffeef0"))
                (magit-diff-removed-highlight (:foreground "#d80d0d" :background "#ffeef0"))

                (magit-header-line (:background nil))
                (magit-blame-culprit (:background "#f6f1e1" :foreground "#b1951d"))
                (magit-blame-date    (:background "#f6f1e1" :foreground "#67b11d"))
                (magit-blame-hash    (:background "#f6f1e1" :foreground "#6c3163"))
                (magit-blame-header  (:background "#f6f1e1" :foreground "#67b11d"))
                (magit-blame-heading (:background "#f6f1e1" :foreground "#67b11d"))
                (magit-blame-name    (:background "#f6f1e1" :foreground "#b1951d"))
                (magit-blame-sha1    (:background "#f6f1e1" :foreground "#6c3163"))
                (magit-blame-subject (:background "#f6f1e1" :foreground "#b1951d"))
                (magit-blame-summary (:background "#f6f1e1" :foreground "#b1951d"))
                (magit-blame-time    (:background "#f6f1e1" :foreground "#67b11d"))

                (diff-header (:background ,bg-darker))
                (diff-file-header (:background ,bg-darker))
                (diff-hunk-header (:background ,bg-darker))
                (diff-added (:foreground "#325e0b"))
                (diff-indicator-added (:foreground "#325e0b"))
                (diff-removed (:foreground "#d80d0d"))
                (diff-indicator-added (:foreground "#d80d0d"))
                (diff-refine-removed (:foreground "#d80d0d" :background "#fdb8c0"))
                (diff-refine-added (:foreground "#325e0b" :background "#acf2bd"))
                (diff-refine-removed (:foreground "#d80d0d" :background "#fdb8c0"))
                (smerge-upper (:foreground "#d80d0d" :background "#fdb8c0"))
                (smerge-lower (:foreground "#325e0b" :background "#acf2bd"))
                (forge-topic-closed (:strike-through t))

                (trailing-whitespace (:background ,bg-accent))
                (ahs-face (:background ,bg-accent-light))
                (ahs-plugin-default-face (:background ,bg-accent-light))
                (ahs-plugin-default-face-unfocused (:inherit ahs-plugin-default-face))
                (ahs-definition-face (:background "#e6ffed"))
                (ahs-plugin-whole-buffer-face (:foreground ,bg-accent :inverse-video t :background ,default-background))
                (evil-ex-lazy-highlight (:background ,bg-accent))
                (evil-ex-search (:background "DarkKhaki"))
                (aw-leading-char-face (:height 5.0))

                (fixed-pitch (:family "JetBrains Mono" :weight normal :width expanded :height 1.0))
                (variable-pitch (:family "Verdana" :weight normal :width expanded))

                ;; (lsp-lens-face (:foreground ,default-foreground))
                (lsp-ui-peek-list (:background "#f0ece1" :foreground "#866f94"))
                (lsp-ui-peek-peek (:background "#f0ece1"))
                (lsp-ui-peek-footer (:background ,bg-accent))
                (lsp-ui-peek-header (:background ,bg-accent))
                (lsp-ui-peek-highlight (:background "#f5f1e6" :foreground "#8b7b96" :box unspecified))
                (lsp-ui-peek-filename (:background "#f0ece1" :foreground "#c0a9cf"))
                (lsp-ui-peek-selection (:background ,bg-accent))

                (lsp-ui-sideline-current-symbol (:foreground ,default-foreground))
                (lsp-ui-sideline-symbol (:foreground ,default-foreground))
                (lsp-ui-sideline-code-action (:foreground "#b7b1ba"))
                (lsp-ui-doc-background (:background "#f0ece1"))
                (markdown-code-face (:foreground ,default-foreground))
                (cider-debug-code-overlay-face (:background "#f0ece1"))
                ;; (lsp-ui-sideline-global (:foreground ,default-foreground))

                ;; org-mode
                (org-block-begin-line (:background ,bg-darker :foreground ,bg-accent-darker :height 0.9 :weight ultra-light :inherit fixed-pitch))
                (org-block (:background ,bg-darker :inherit fixed-pitch))
                (org-block-end-line (:background ,bg-darker :foreground ,bg-accent-darker :height 0.9 :weight ultra-light :inherit fixed-pitch))
                (org-modern-indent-bracket-line (:background ,bg-darker :height 1.3 :inherit org-meta-line))
                (org-verse (:inherit (variable-pitch org-block)))
                (org-quote (:inherit (org-verse) :slant normal))
                (org-date (:inherit fixed-pitch))
                (org-code (:inherit fixed-pitch :foreground ,fg-accent))
                (org-verbatim (:inherit fixed-pitch))
                (info-quoted-name (:inherit fixed-pitch))
                (org-meta-line (:inherit fixed-pitch :foreground ,bg-accent :height 0.9 :weight ultra-light))
                (org-special-keyword (:inherit fixed-pitch))
                (org-checkbox (:inherit fixed-pitch))
                (org-table (:inherit fixed-pitch))
                (org-done  (:foreground ,bg-accent))
                (org-headline-done (:foreground ,bg-accent-dark))
                (org-hide (:foreground ,default-background))
                (org-indent (:inherit (fixed-pitch org-hide)))
                (org-roam-link (:background "#fff9de"))
                (org-roam-link-invalid (:background "#fbf3ef"))
                (org-drawer (:foreground ,bg-accent-dark))
                (org-special-keyword (:inherit (org-drawer) :foreground ,bg-accent-dark))
                (org-property-value (:inherit (org-drawer) :foreground ,bg-accent-dark))

                (gnus-cite-1 (:foreground "SkyBlue3"))
                (gnus-cite-2 (:foreground "light sky blue"))
                (gnus-cite-3 (:foreground "yellow3"))
                (mm-uu-extract (:background "#efeae9"))

                (cider-debug-code-overlay-face (:background ,bg-darker))
                (ivy-posframe (:background (darker default :background 1)))
                (ivy-posframe-border (:background (lighter default :foreground 5)))

                (notmuch-wash-cited-text (:foreground ,bg-accent-darker))
                (message-header-to (:foreground ,fg-accent))
                (notmuch-crypto-signature-unknown (:foreground "#fdb8c0" :background unspecified))
                (notmuch-crypto-signature-good-key (:foreground "DarkKhaki" :background unspecified))

                (code-review-pending-state-face (:foreground "DarkKhaki"))
                (code-review-request-review-face (:foreground "yellow3"))
                (completions-annotations (:foreground ,bg-accent-darker))

                (elfeed-search-title-face (:foreground ,bg-accent-dark))
                (elfeed-search-feed-face (:foreground "wheat3"))

                (yas-field-highlight-face (:inherit match))
                (show-paren-match (:background unspecified))
                (error (:foreground "#fdb8c0"))
                (vertico-current (:background ,bg-accent))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-spacemacs-light
                         'spacemacs-light
                         new-faces))

(provide-theme 'ag-themes-spacemacs-light)

(provide 'ag-themes-spacemacs-light-theme)
;;; ag-themes-spacemacs-light-theme.el ends here
