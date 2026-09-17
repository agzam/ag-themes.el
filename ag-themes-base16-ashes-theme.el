;;; ag-themes-base16-ashes-theme.el --- Customized theme based on base16-ashes -*- lexical-binding: t; -*-
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
;; Customized theme based on base16-ashes.
;;
;;; Code:

(require 'ag-themes)

(ag-themes-deftheme ag-themes-base16-ashes
  "Customized theme based on base16-ashes."
  :base base16-ashes
  :modeline-height 0.85
  :palette ((bg-accent . "#3a4a4f")
            (diff-green . "#598d48")
            (diff-green-fg . "#96b388")
            (diff-green-hl . "#b1caa4")
            (diff-red . "#994b4e")
            (diff-red-fg . "#bc898d")
            (diff-red-hl . "#d4a4a6")
            (diff-amber . "#99824b")
            (diff-amber-fg . "#b6a785")
            (diff-amber-hl . "#cfc3a0"))
  :faces
  (show-paren-match :background unspecified :foreground "orange"

   ;; Magit's own defface supplies a saturated green and red whenever a theme
   ;; leaves these unset, so every tint is explicit here.  Each one is mixed
   ;; into this theme's background, which keeps it off the eye.
   magit-diff-added :foreground diff-green-fg
                    :background (blend 13 diff-green default :background)
   magit-diff-added-highlight :foreground diff-green-hl
                              :background (blend 20 diff-green default :background)
   magit-diff-removed :foreground diff-red-fg
                      :background (blend 13 diff-red default :background)
   magit-diff-removed-highlight :foreground diff-red-hl
                                :background (blend 20 diff-red default :background)
   magit-diff-context-highlight :background (lighter 4 default :background)
   magit-section-highlight :background (lighter 4 default :background)
   magit-diff-hunk-heading :background (lighter 9 default :background)
                           :foreground unspecified
   magit-diff-hunk-heading-highlight :background (lighter 15 default :background)
                                     :foreground unspecified
   magit-diff-file-heading-highlight :background (lighter 15 default :background)
                                     :foreground unspecified

   diff-header :background (lighter 9 default :background)
   diff-file-header :background (lighter 9 default :background)
   diff-hunk-header :background (lighter 9 default :background)
   diff-added :foreground diff-green-fg
              :background (blend 13 diff-green default :background)
   diff-removed :foreground diff-red-fg
                :background (blend 13 diff-red default :background)
   diff-indicator-added :foreground diff-green-hl
                        :background (blend 13 diff-green default :background)
   diff-indicator-removed :foreground diff-red-hl
                          :background (blend 13 diff-red default :background)
   diff-refine-added :foreground diff-green-hl
                     :background (blend 38 diff-green default :background)
   diff-refine-removed :foreground diff-red-hl
                       :background (blend 38 diff-red default :background)

   smerge-upper :background (blend 13 diff-red default :background)
   smerge-lower :background (blend 13 diff-green default :background)
   smerge-refined-added :foreground diff-green-hl
                        :background (blend 38 diff-green default :background)
   smerge-refined-removed :foreground diff-red-hl
                          :background (blend 38 diff-red default :background)

   ediff-current-diff-A :foreground diff-red-hl
                        :background (blend 18 diff-red default :background)
   ediff-fine-diff-A :foreground diff-red-hl
                     :background (blend 38 diff-red default :background)
   ediff-current-diff-B :foreground diff-green-hl
                        :background (blend 18 diff-green default :background)
   ediff-fine-diff-B :foreground diff-green-hl
                     :background (blend 38 diff-green default :background)

   ;; Conflict and whitespace faces default to near-white slabs on a dark
   ;; background; amber carries the base side, as it does on GitHub.
   magit-diff-base :foreground diff-amber-fg
                   :background (blend 13 diff-amber default :background)
   magit-diff-base-highlight :foreground diff-amber-hl
                             :background (blend 20 diff-amber default :background)
   magit-diff-our-heading :background (blend 20 diff-red default :background)
                          :foreground unspecified
   magit-diff-their-heading :background (blend 20 diff-green default :background)
                            :foreground unspecified
   magit-diff-base-heading :background (blend 20 diff-amber default :background)
                           :foreground unspecified
   magit-diff-conflict-heading :background (lighter 9 default :background)
                               :foreground unspecified
   magit-diff-whitespace-warning :background (blend 40 diff-red default :background)
                                 :foreground unspecified
   magit-diff-lines-heading :background (blend 45 diff-red default :background)
                            :foreground unspecified
   magit-diff-lines-boundary :background (blend 45 diff-red default :background)

   diff-changed :foreground diff-amber-fg
                :background (blend 13 diff-amber default :background)
   diff-changed-unspecified :foreground diff-amber-fg
                            :background (blend 13 diff-amber default :background)
   diff-indicator-changed :foreground diff-amber-hl
                          :background (blend 13 diff-amber default :background)
   diff-refine-changed :foreground diff-amber-hl
                       :background (blend 38 diff-amber default :background)

   smerge-base :background (blend 13 diff-amber default :background)
   smerge-refined-changed :background (blend 38 diff-amber default :background)
   smerge-markers :background (lighter 9 default :background)
                  :foreground unspecified

   tab-bar :background (lighter 0 default :background)
   tab-bar-tab :background (darker 10 default :background)
               :foreground (darker 20 default :foreground)
               :weight bold :box unspecified
   tab-bar-tab-inactive :background (lighter 0 default :background)
                        :foreground (darker 35 default :foreground)
                        :box unspecified
   vertico-current :background bg-accent))

;;; ag-themes-base16-ashes-theme.el ends here
