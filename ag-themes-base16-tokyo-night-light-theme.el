;;; ag-themes-base16-tokyo-night-light-theme.el --- Customized theme based on base16-tokyo-night-light -*- lexical-binding: t; -*-
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
;; Customized theme based on base16-tokyo-night-light.
;;
;;; Code:

(require 'ag-themes)

(ag-themes-deftheme ag-themes-base16-tokyo-night-light
  "Customized theme based on base16-tokyo-night-light."
  :base base16-tokyo-night-light
  :modeline-height 0.85
  :palette ((base01 . "#cbccd1")
            (base04 . "#4c505e")
            (diff-add-bg . "#e6ffed")
            (diff-add-word . "#acf2bd")
            (diff-add-fg . "#1a7f37")
            (diff-add-fg-hl . "#116329")
            (diff-del-bg . "#ffeef0")
            (diff-del-word . "#fdb8c0")
            (diff-del-fg . "#cf222e")
            (diff-del-fg-hl . "#a40e26")
            (diff-chg-bg . "#fff8c5")
            (diff-chg-word . "#ffe57f")
            (diff-chg-fg . "#9a6700")
            (diff-chg-fg-hl . "#7d4e00"))
  :faces
  (default :background (lighter 0)
   show-paren-match :background unspecified
                    :foreground (lighter 0 cursor :background)
                    :underline t
   dired-directory :background (lighter 1.5 default :background)
   dired-subtree-depth-1-face :background (lighter 5 dired-directory :background)
   dired-subtree-depth-2-face :background (lighter 8 dired-directory :background)
   dired-subtree-depth-3-face :background (lighter 12 dired-directory :background)
   dired-subtree-depth-4-face :background (lighter 15 dired-directory :background)
   dired-subtree-depth-5-face :background (lighter 20 dired-directory :background)
   dired-subtree-depth-6-face :background (lighter 30 dired-directory :background)
   aw-leading-char-face :height 5.0
   org-meta-line :foreground "#b6b9c4"
   org-block-begin-line :extend t
   org-block-end-line :extend t :inherit org-block-begin-line
   org-modern-indent-bracket-line :background base01 :height 1.3
                                  :inherit org-meta-line
   lsp-face-highlight-textual :background base04
   vertico-current :background base01

   ;; The base theme leaves the Magit diff faces unset, so Emacs' own saturated
   ;; defaults show through.  GitHub's light tints, mixed into this theme's own
   ;; background so they keep its cast.
   magit-diff-added :foreground diff-add-fg
                    :background (blend 70 diff-add-bg default :background)
   magit-diff-added-highlight :foreground diff-add-fg-hl
                              :background (blend 70 diff-add-bg default :background)
   magit-diff-removed :foreground diff-del-fg
                      :background (blend 70 diff-del-bg default :background)
   magit-diff-removed-highlight :foreground diff-del-fg-hl
                                :background (blend 70 diff-del-bg default :background)
   magit-diff-context-highlight :background (darker 3 default :background)
   magit-section-highlight :background (darker 3 default :background)
   magit-diff-hunk-heading :background (darker 5 default :background)
                           :foreground unspecified
   magit-diff-hunk-heading-highlight :background (darker 9 default :background)
                                     :foreground unspecified
   magit-diff-file-heading-highlight :background (darker 9 default :background)
                                     :foreground unspecified

   diff-header :background (darker 5 default :background)
   diff-file-header :background (darker 5 default :background)
   diff-hunk-header :background (darker 5 default :background)
   diff-added :foreground diff-add-fg-hl
              :background (blend 70 diff-add-bg default :background)
   diff-removed :foreground diff-del-fg-hl
                :background (blend 70 diff-del-bg default :background)
   diff-indicator-added :foreground diff-add-fg-hl :background unspecified
   diff-indicator-removed :foreground diff-del-fg-hl :background unspecified
   diff-refine-added :foreground diff-add-fg-hl
                     :background (blend 70 diff-add-word default :background)
   diff-refine-removed :foreground diff-del-fg-hl
                       :background (blend 70 diff-del-word default :background)

   smerge-upper :background (blend 70 diff-del-bg default :background)
   smerge-lower :background (blend 70 diff-add-bg default :background)
   smerge-refined-added :foreground diff-add-fg-hl
                        :background (blend 70 diff-add-word default :background)
   smerge-refined-removed :foreground diff-del-fg-hl
                          :background (blend 70 diff-del-word default :background)

   ediff-current-diff-A :foreground diff-del-fg-hl
                        :background (blend 70 diff-del-bg default :background)
   ediff-fine-diff-A :foreground diff-del-fg-hl
                     :background (blend 70 diff-del-word default :background)
   ediff-current-diff-B :foreground diff-add-fg-hl
                        :background (blend 70 diff-add-bg default :background)
   ediff-fine-diff-B :foreground diff-add-fg-hl
                     :background (blend 70 diff-add-word default :background)
   ediff-current-diff-C :foreground diff-chg-fg-hl
                        :background (blend 70 diff-chg-bg default :background)
   ediff-fine-diff-C :foreground diff-chg-fg-hl
                     :background (blend 70 diff-chg-word default :background)
   ediff-current-diff-Ancestor :foreground diff-chg-fg-hl
                               :background (blend 70 diff-chg-bg default :background)
   ediff-fine-diff-Ancestor :foreground diff-chg-fg-hl
                            :background (blend 70 diff-chg-word default :background)

   ;; The non-current regions are neutral bands, even a step lighter than odd.
   ;; Both need an explicit pair: base themes that inverted them named no
   ;; colour, and an unset one falls through to a grey `defface' slab.
   ediff-even-diff-A :foreground (lighter 0 default :foreground)
                     :background (darker 3 default :background)
   ediff-even-diff-B :foreground (lighter 0 default :foreground)
                     :background (darker 3 default :background)
   ediff-even-diff-C :foreground (lighter 0 default :foreground)
                     :background (darker 3 default :background)
   ediff-even-diff-Ancestor :foreground (lighter 0 default :foreground)
                            :background (darker 3 default :background)
   ediff-odd-diff-A :foreground (lighter 0 default :foreground)
                    :background (darker 7 default :background)
   ediff-odd-diff-B :foreground (lighter 0 default :foreground)
                    :background (darker 7 default :background)
   ediff-odd-diff-C :foreground (lighter 0 default :foreground)
                    :background (darker 7 default :background)
   ediff-odd-diff-Ancestor :foreground (lighter 0 default :foreground)
                           :background (darker 7 default :background)

   ;; Conflict and whitespace faces default to saturated slabs; amber carries
   ;; the base side, as it does on GitHub.
   magit-diff-base :foreground diff-chg-fg
                   :background (blend 70 diff-chg-bg default :background)
   magit-diff-base-highlight :foreground diff-chg-fg-hl
                             :background (blend 70 diff-chg-bg default :background)
   magit-diff-our-heading :background (blend 70 diff-del-bg default :background)
                          :foreground unspecified
   magit-diff-their-heading :background (blend 70 diff-add-bg default :background)
                            :foreground unspecified
   magit-diff-base-heading :background (blend 70 diff-chg-bg default :background)
                           :foreground unspecified
   magit-diff-conflict-heading :background (darker 5 default :background)
                               :foreground unspecified
   magit-diff-whitespace-warning :background (blend 70 diff-del-word default :background)
                                 :foreground unspecified
   magit-diff-lines-heading :background (blend 70 diff-del-word default :background)
                            :foreground unspecified
   magit-diff-lines-boundary :background (blend 70 diff-del-word default :background)

   diff-changed :foreground diff-chg-fg-hl
                :background (blend 70 diff-chg-bg default :background)
   diff-changed-unspecified :foreground diff-chg-fg-hl
                            :background (blend 70 diff-chg-bg default :background)
   diff-indicator-changed :foreground diff-chg-fg-hl
                          :background (blend 70 diff-chg-bg default :background)
   diff-refine-changed :foreground diff-chg-fg-hl
                       :background (blend 70 diff-chg-word default :background)

   smerge-base :background (blend 70 diff-chg-bg default :background)
   smerge-refined-changed :background (blend 70 diff-chg-word default :background)
   smerge-markers :background (darker 5 default :background)
                  :foreground unspecified))

;;; ag-themes-base16-tokyo-night-light-theme.el ends here
