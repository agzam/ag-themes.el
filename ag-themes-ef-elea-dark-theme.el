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
   font-lock-warning-face :background unspecified))

;;; ag-themes-ef-elea-dark-theme.el ends here
