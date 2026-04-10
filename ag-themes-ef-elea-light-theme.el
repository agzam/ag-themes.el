;;; ag-themes-ef-elea-light-theme.el --- Customized theme based on ef-elea-light -*- lexical-binding: t; -*-
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
;; Customized theme based on ef-elea-light.
;;
;;; Code:

(require 'ag-themes)
(require 'ef-themes)

(ag-themes-deftheme ag-themes-ef-elea-light
  "Customized theme based on ef-elea-light."
  :base ef-elea-light
  :modeline-height 0.85
  :faces
  (default :background (lighter 0)

   region :background (darker 7 default :background)
          :inverse-video unspecified

   tab-bar :background (lighter 3 default :background)
   tab-bar-tab :background (lighter 20 default :background)
               :foreground (lighter 0 default :foreground)
               :weight bold :box unspecified
   tab-bar-tab-inactive :background (lighter 3 default :background)
                        :foreground (darker 20 default :foreground)
                        :box unspecified

   org-block-begin-line :underline unspecified
                        :background (darker 3 default :background)
                        :foreground (darker 60 default :background)
                        :height 0.9 :weight ultra-light
                        :inherit fixed-pitch :extend t

   org-block :background unspecified :inherit fixed-pitch

   org-block-end-line :overline unspecified
                      :background (darker 3 default :background)
                      :foreground (darker 60 default :background)
                      :height 0.9 :weight ultra-light
                      :inherit fixed-pitch :extend t

   org-meta-line :inherit fixed-pitch :height 0.9 :weight ultra-light

   org-modern-indent-bracket-line :background (darker 3 default :background)
                                  :foreground (darker 20 default :background)
                                  :height 1.3 :inherit org-meta-line

   org-drawer :foreground (darker 15 default :background)
   org-special-keyword :inherit (org-drawer fixed-pitch)
                       :foreground (darker 28 default :background)
   org-property-value :inherit (org-drawer fixed-pitch)
                      :foreground (darker 32 default :background)

   magit-diff-hunk-heading-highlight :background unspecified
   magit-section-highlight :background unspecified
   magit-diff-context-highlight :background unspecified
   help-key-binding :background unspecified))

;;; ag-themes-ef-elea-light-theme.el ends here
