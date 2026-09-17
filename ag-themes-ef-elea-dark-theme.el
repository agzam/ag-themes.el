;;; ag-themes-ef-elea-dark-theme.el --- Customized theme based on ef-elea-dark -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/ag-themes.el
;; Created: Nov-2021
;; Keywords: faces
;; License: GPL v3
;; Package-Requires: ((emacs "30") (ef-themes "1"))
;; Version: 1.0.0
;;
;;; Commentary:
;;
;; Customized theme based on ef-elea-dark.
;;
;;; Code:

(require 'ag-themes)
(require 'ef-themes)

(ag-themes-deftheme ag-themes-ef-elea-dark
  "Customized theme based on ef-elea-dark."
  :base ef-elea-dark
  :modeline-height 0.85
  :palette ((diff-green . "#598d48")
            (diff-green-fg . "#96b388")
            (diff-green-hl . "#b1caa4")
            (diff-red . "#994b4e")
            (diff-red-fg . "#bc898d")
            (diff-red-hl . "#d4a4a6")
            (diff-amber . "#99824b")
            (diff-amber-fg . "#b6a785")
            (diff-amber-hl . "#cfc3a0"))
  :faces
  (default :background (lighter 0)

   tab-bar :background unspecified
   tab-bar-tab :background unspecified
               :foreground (lighter 0 default :foreground)
               :weight bold :box unspecified
   tab-bar-tab-inactive :background (lighter 3 default :background)
                        :foreground (darker 20 default :foreground)
                        :box unspecified

   org-block-begin-line :underline unspecified
                        :background (lighter 3 default :background)
                        :foreground (lighter 60 default :background)
                        :height 0.9 :weight ultra-light
                        :inherit fixed-pitch :extend t

   org-block :background unspecified :inherit fixed-pitch

   org-block-end-line :overline unspecified
                      :background (darker 0 org-block-begin-line :background)
                      :foreground (darker 0 org-block-begin-line :foreground)
                      :height 0.9 :weight ultra-light
                      :inherit fixed-pitch :extend t

   org-modern-indent-bracket-line :background (darker 3 org-block-begin-line :background)
                                  :foreground (lighter 20 default :background)
                                  :height 1.3 :inherit org-meta-line

   dired-directory :background unspecified
   dired-subtree-depth-1-face :background unspecified
   dired-subtree-depth-2-face :background unspecified
   dired-subtree-depth-3-face :background unspecified
   dired-subtree-depth-4-face :background unspecified
   dired-subtree-depth-5-face :background unspecified
   dired-subtree-depth-6-face :background unspecified
   dired-symlink :background unspecified

   font-lock-comment-face :background (darker 5 default :background)
                          :foreground (lighter 25 default :background)
   font-lock-comment-delimiter-face :background (darker 5 default :background)
                                    :foreground (lighter 30 default :background)
   font-lock-warning-face :background unspecified

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
                  :foreground unspecified))

;;; ag-themes-ef-elea-dark-theme.el ends here
