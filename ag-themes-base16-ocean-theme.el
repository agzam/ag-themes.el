;;; ag-themes-base16-ocean-theme.el --- Customized theme based on base16-ocean -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: color theme
;; License: GPL v3
;; Package-Requires: ((emacs "27"))
;; Version: 1.0.0

(require 'ag-themes)
(require 'base16-ocean-theme)

(deftheme ag-themes-base16-ocean "Customized theme based on base16-ocean")

(let* ((base-faces (color-theme-get-faces 'base16-ocean))
       (base00 "#242938")
       (base00*1 "#212533")
       (base01 "#343d46")
       (base02 "#4f5b66")
       (base03 "#65737e")
       (base04 "#a7adba")
       (base05 "#c0c5ce")
       (base06 "#dfe1e8")
       (base07 "#eff1f5")
       (base08 "#bf616a")
       (base09 "#d08770")
       (base0A "#ebcb8b")
       (base0B "#a3be8c")
       (base0C "#96b5b4")
       (base0D "#8fa1b3")
       (base0E "#b48ead")
       (base0F "#ab7967")
       (base10 "#7090af")
       (default-background base00)
       (default-foreground base05)
       (alter `((default (:background (lighter 1.5)))
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

                (region (:inverse-video t :foreground ,base03 :background ,default-background :distant-foreground nil))
                (fixed-pitch (:family "JetBrains Mono" :weight normal :width expanded))
                (variable-pitch (:family "Open Sans" :weight normal
                                        :width expanded :height 1.2))
                ;; avy
                (aw-leading-char-face (:height 5.0 :foreground "Orange"))
                (avy-lead-face (:height 1.3 :foreground ,base0A))
                (avy-lead-face-0 (:height 1.3 :foreground ,base09))
                (avy-lead-face-1 (:height 1.3 :foreground ,base0C))
                (avy-lead-face-2 (:height 1.3 :foreground ,base10))

                ;; org-mode
                (org-link (:underline t :foreground ,base0B))
                (org-todo (:weight bold :foreground ,base0A :inherit fixed-pitch))
                (org-block-begin-line (:underline nil :background ,base01 :foreground ,base04 :height 0.9 :weight ultra-light :inherit fixed-pitch))
                (org-block (:background ,base01 :inherit fixed-pitch))
                (org-block-end-line (:overline nil :background ,base01 :height 0.9 :weight ultra-light :inherit fixed-pitch))
                (org-verse (:inherit (variable-pitch org-block)))
                (org-quote (:inherit (org-verse) :slant normal))
                (org-date (:inherit fixed-pitch))
                (org-code (:inherit fixed-pitch :foreground ,base06))
                (org-verbatim (:inherit fixed-pitch))
                (org-meta-line (:inherit fixed-pitch :foreground ,base04 :height 0.9 :weight ultra-light))
                (org-checkbox (:background nil :inherit fixed-pitch))
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

                ;; misc
                (hl-line (:background "#2f3440"))
                (trailing-whitespace (:background ,base01))
                (mode-line (:underline (:color ,base01)))
                (mode-line-inactive (:underline (:color ,base01)))

                (line-number (:inherit fixed-pitch))
                (ivy-posframe (:inherit default))

                (notmuch-wash-cited-text (:foreground ,base03))
                (message-header-to (:foreground ,base0C))
                (notmuch-crypto-signature-unknown (:foreground ,base0E :background nil))
                (notmuch-crypto-signature-good-key (:foreground ,base0A :background nil)))))
  (dolist (face (append alter (ag-themes--modify-modeline-faces '(:height 0.8))))
    (add-to-list 'base-faces face :append))
  (color-theme-set-faces
   'ag-themes-base16-ocean base-faces))

(provide-theme 'ag-themes-base16-ocean)

(provide 'ag-themes-base16-ocean-theme)
