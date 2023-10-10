;;; ag-themes-base16-ocean-theme.el --- Customized theme based on base16-ocean -*- lexical-binding: t; -*-
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
;; Customized theme based on base-16-ocean
;;; Code:

(require 'ag-themes)
(require 'base16-ocean-theme)

(deftheme ag-themes-base16-ocean "Customized theme based on base16-ocean.")

(let* ((base00 "#2e323c")
       (base01 "#343d46")
       (base02 "#5b6a78")
       (base03 "#65737e")
       (base04 "#a7adba")
       ;; (base05 "#c0c5ce")
       (base06 "#dfe1e8")
       (base07 "#eff1f5")
       (base08 "#bf616a")
       (base09 "#d08770")
       (base0A "#ebcb8b")
       (base0B "#a3be8c")
       (base0C "#96b5b4")
       (base0D "#8fa1b3")
       (base0E "#b48ead")
       ;; (base0F "#ab7967")
       (base10 "#7090af")
       (default-background base00)
       ;; (default-foreground base05)
       (faces `((default (:background (lighter 5)))
                (window-divider (:foreground (lighter default :background 0)))
                (window-divider-first-pixel (:foreground (lighter default :background 0)))
                (window-divider-last-pixel (:foreground (lighter default :background 0)))
                (mode-line (:underline unspecified :background (lighter default :background 3)))
                (mode-line-inactive (:underline nil :background (lighter default :background 5)))
                (doom-modeline-bar (:background (lighter default :background 20)))
                (doom-modeline-bar-inactive (:inherit fringe))
                (cursor (:background "DarkGoldenrod2"))
                (show-paren-match (:background unspecified :foreground (lighter cursor :background 0) :underline t))
                (dired-directory (:background (lighter default :background 1.5)))
                (dired-subtree-depth-1-face (:background (lighter dired-directory :background 5)))
                (dired-subtree-depth-2-face (:background (lighter dired-directory :background 8)))
                (dired-subtree-depth-3-face (:background (lighter dired-directory :background 12)))
                (dired-subtree-depth-4-face (:background (lighter dired-directory :background 15)))
                (dired-subtree-depth-5-face (:background (lighter dired-directory :background 20)))
                (dired-subtree-depth-6-face (:background (lighter dired-directory :background 30)))

                ;; magit
                (magit-popup-disabled-argument (:foreground ,base02))
                (magit-popup-option-value (:foreground ,base08))
                (magit-popup-argument (:foreground ,base08))

                (magit-diff-context-highlight (:background ,default-background))
                (magit-diff-removed (:foreground ,base08))
                (magit-diff-added (:foreground ,base0B))
                (magit-diff-removed-highlight (:foreground "#ef6160"))
                (magit-diff-added-highlight (:foreground "#a3be70"))
                (magit-section-highlight (:background "#2f343f"))
                (magit-diff-hunk-heading (:background "#2f343f"))
                (magit-diff-hunk-heading-highlight (:background "#2f363f"))
                (diff-refine-added (:foreground "#a3be70" :background "#2b3b34"))
                (diff-refine-removed (:foreground "#ef6160" :background "#3b2c2b"))
                (smerge-refined-added (:foreground "#a3be70" :background "#2b3b34")) (smerge-refined-removed (:foreground "#ef6160" :background "#3b2c2b")) (forge-topic-closed (:strike-through t))
                (ediff-current-diff-A (:foreground "#dd828b" :background "#443238"))
                (ediff-fine-diff-A (:foreground "#db5e6c" :background "#603238"))
                (ediff-current-diff-B (:foreground ,base0B :background "#2a3a2c"))
                (ediff-fine-diff-B (:foreground "#aadd7e" :background "#2e4431"))

                ;; diff-hl
                (diff-hl-change (:foreground ,base03 :background ,base0D))
                (diff-hl-delete (:foreground ,base03 :background ,base08))
                (diff-hl-insert (:foreground ,base03 :background ,base0B))
                (diff-hl-unknown (:foreground ,base03 :background ,base0A))

                (ahs-plugin-whole-buffer-face (:foreground ,base0B :background ,default-background))
                (ahs-plugin-default-face (:foreground ,base0A :background ,base02))
                (ahs-plugin-default-face-unfocused (:inherit ahs-plugin-default-face))
                (ahs-definition-face (:foreground ,base0A :background ,base03))
                (ahs-face (:foreground ,base0A :background ,base02))

                (region (:inverse-video t :foreground ,base03 :background ,default-background :distant-foreground unspecified))
                (fixed-pitch (:family "JetBrains Mono" :weight normal :width expanded :height unspecified))
                (variable-pitch (:family "Verdana" :weight normal :width expanded))
                ;; avy
                (aw-leading-char-face (:height 5.0 :foreground "Orange"))
                (avy-lead-face (:height 1.3 :foreground ,base0A))
                (avy-lead-face-0 (:height 1.3 :foreground ,base09))
                (avy-lead-face-1 (:height 1.3 :foreground ,base0C))
                (avy-lead-face-2 (:height 1.3 :foreground ,base10))

                ;; org-mode
                (org-link (:underline t :foreground ,base0B))
                (org-todo (:weight bold :foreground ,base0A :inherit fixed-pitch))
                (org-block-begin-line (:underline unspecified :background ,base01 :foreground ,base04 :height 0.9 :weight ultra-light :inherit fixed-pitch))
                (org-block (:background ,base01 :inherit fixed-pitch))
                (org-block-end-line (:overline unspecified :background ,base01 :height 0.9 :weight ultra-light :inherit fixed-pitch))
                (org-verse (:inherit (variable-pitch org-block)))
                (org-quote (:inherit (org-verse) :slant normal))
                (org-date (:inherit fixed-pitch))
                (org-code (:inherit fixed-pitch :foreground ,base06))
                (org-verbatim (:inherit fixed-pitch))
                (org-meta-line (:inherit fixed-pitch :foreground ,base04 :height 0.9 :weight ultra-light))
                (org-checkbox (:background unspecified :inherit fixed-pitch))
                (org-table (:inherit fixed-pitch))
                (org-level-1 (:foreground ,base0D :bold t :height 1.3))
                (org-level-2 (:foreground ,base09 :bold t :height 1.2))
                (org-level-3 (:foreground ,base0B :height 1.1))
                (org-level-4 (:foreground ,base10 :height 1.0))
                (org-level-5 (:foreground ,base0E :height 1.0))
                (org-level-6 (:foreground ,base0C :height 1.0))
                (org-level-7 (:foreground ,base07 :height 1.0))
                (org-level-8 (:foreground ,base0D :height 1.0))
                (org-done (:foreground ,base02))
                (org-headline-done (:foreground ,base02))
                (org-hide (:foreground ,default-background))
                (org-indent (:inherit (fixed-pitch org-hide)))
                (org-roam-link (:background ,base01 :foreground ,base0D))
                (org-roam-link-invalid (:background "#45352e"))
                (org-drawer (:foreground ,base02))
                (org-special-keyword (:inherit (org-drawer fixed-pitch) :foreground ,base02))
                (org-property-value (:inherit (org-drawer fixed-pitch) :foreground ,base02))

                ;; code
                (font-lock-doc-face (:foreground ,base02))

                (term-color-blue (:foreground (lighter default :foreground 2)))
                (ansi-color-blue (:foreground "#00bfff" :background "#00bfff"))

                ;; misc
                (hl-line (:background "#2f3440"))
                (trailing-whitespace (:background ,base01))

                (line-number (:inherit fixed-pitch))
                (ivy-posframe (:inherit default))

                (notmuch-wash-cited-text (:foreground ,base03))
                (message-header-to (:foreground ,base0C))
                (notmuch-crypto-signature-unknown (:foreground ,base0E :background unspecified))
                (notmuch-crypto-signature-good-key (:foreground ,base0A :background unspecified))

                (tab-bar (:background (lighter default :background 3)))
                (tab-bar-tab (:background (lighter default :background 20)
                              :foreground (lighter default :foreground 0)
                              :weight bold :box unspecified))
                (tab-bar-tab-inactive (:background (lighter default :background 3)
                                       :foreground (darker default :foreground 20)))

                (lsp-lsp-flycheck-warning-unnecessary-face (:foreground ,base03 :background ,base01))

                (elfeed-search-title-face (:foreground ,base03))
                (elfeed-search-unread-title-face (:foreground ,base04))
                (elfeed-search-feed-face (:foreground ,base0A))
                (elfeed-search-tag-face (:foreground ,base0B))))
       (new-faces (append faces (ag-themes--modify-modeline-faces '(:height 0.85)))))
  (color-theme-set-faces 'ag-themes-base16-ocean
                         'base16-ocean
                         new-faces))


(provide-theme 'ag-themes-base16-ocean)

(provide 'ag-themes-base16-ocean-theme)
;;; ag-themes-base16-ocean-theme.el ends here
