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
            (base04 . "#4c505e"))
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
   vertico-current :background base01))

;;; ag-themes-base16-tokyo-night-light-theme.el ends here
